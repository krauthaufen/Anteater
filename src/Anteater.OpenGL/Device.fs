namespace Anteater.OpenGL

open System
open System.Threading
open System.Runtime.InteropServices
open System.Threading.Tasks
open System.Collections.Concurrent
open Aardvark.Base
open Silk.NET.OpenGL
open Aardvark.Base.Runtime
open Microsoft.FSharp.NativeInterop
open Silk.NET.OpenGL.Extensions.ARB
open Anteater
open System.Runtime.CompilerServices
open Silk.NET.Core.Native

#nowarn "9"

[<StructuredFormatDisplay("{AsString}")>]
type OpenGLFeatures =
    {
        version         : Version
        directState     : bool
        copyBuffer      : bool
        copyImage       : bool
        bufferStorage   : bool
        textureStorage  : bool
    }

    member private x.AsString = x.ToString()
    override x.ToString() =
        let exts =
            [
                if x.directState then yield "DirectStateAccess"
                if x.copyBuffer then yield "CopyBuffer"
                if x.copyImage then yield "CopyImage"
                if x.bufferStorage then yield "BufferStorage"
                if x.textureStorage then yield "TextureStorage"
            ]

        match exts with
        | [] -> sprintf "OpenGL %A" x.version
        | _ -> String.concat "; " exts |> sprintf "OpenGL %A { %s }" x.version

    
    static member None =
        {
            version         = Version(4,1)
            directState     = false
            copyBuffer      = false
            copyImage       = false
            bufferStorage   = false
            textureStorage  = false
        }

    static member Default =
        {
            version         = Version(4,1)
            directState     = true
            copyBuffer      = true
            copyImage       = true
            bufferStorage   = true
            textureStorage  = true
        }

type DeviceConfig =
    {
        nVidia      : bool
        queues      : int
        features    : OpenGLFeatures
        debug       : bool
    }

type DeviceInfo =
    {
        vendor      : string
        renderer    : string
        version     : Version
        glsl        : Version

        extensions  : Set<string>
        features    : OpenGLFeatures
    }


module internal DeviceInfo =
    open System.Text.RegularExpressions

    // major_number.minor_number major_number.minor_number.release_number

    let versionRx = Regex @"^([0-9]+)[ \t]*\.[ \t]*([0-9]+)([ \t]*\.[ \t]*([0-9]+))?"



    let read (gl : GL) (cfg : DeviceConfig) =

        let glsl =
            let glsl = gl.GetString(StringName.ShadingLanguageVersion)
            let m = versionRx.Match glsl
            if m.Success then
                if m.Groups.[3].Success then 
                    Version(
                        m.Groups.[1].Value |> int,
                        m.Groups.[2].Value |> int,
                        m.Groups.[4].Value |> int
                    )
                else
                    Version(
                        m.Groups.[1].Value |> int,
                        m.Groups.[2].Value |> int
                    )
            else
                Version(0, 0)

        let glsl = Version(glsl.Major, glsl.Minor / 10, glsl.Minor % 10)
        let version = Version(gl.GetInteger(GetPName.MajorVersion), gl.GetInteger(GetPName.MinorVersion))
        let extensions = Seq.init (gl.GetInteger GetPName.NumExtensions) (fun i -> gl.GetString(StringName.Extensions, uint32 i)) |> Set.ofSeq
        {
            vendor = gl.GetString(StringName.Vendor)
            renderer = gl.GetString(StringName.Renderer)
            version = version
            glsl = glsl

            extensions = extensions
            features = 
                {
                    version = version
                    directState = cfg.features.directState && fst (gl.TryGetExtension<ArbDirectStateAccess>())
                    copyBuffer = cfg.features.copyBuffer && fst (gl.TryGetExtension<ArbCopyBuffer>())
                    copyImage = cfg.features.copyImage && fst (gl.TryGetExtension<ArbCopyImage>())
                    bufferStorage = cfg.features.bufferStorage && fst (gl.TryGetExtension<ArbBufferStorage>())
                    textureStorage = cfg.features.textureStorage && fst (gl.TryGetExtension<ArbTextureStorage>()) && fst (gl.TryGetExtension<ArbTextureStorageMultisample>())
                }
        }

type internal CommandStreamScoreAttribute(value : int) =
    inherit System.Attribute()
    member x.Score = value

[<AbstractClass>]
type internal OpenGLCommandStream() =
    inherit CommandStream()
    abstract member Run : ContextHandle * GL -> unit

module internal Reflection =
    open System.Reflection
    open System.Reflection.Emit

    type private CommandStreamCreator = delegate of obj -> OpenGLCommandStream

    let ctor (c : ConstructorInfo) : option<obj -> OpenGLCommandStream> =
        let compatible = 
            let prop = c.DeclaringType.GetProperty("IsCompatible", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
            if isNull prop || prop.PropertyType <> typeof<bool> then 
                true
            else
                let v = prop.GetValue(null) |> unbox<bool>
                v
        if compatible then
            try
                let meth =
                    DynamicMethod(
                        sprintf "create%A" c.DeclaringType,
                        typeof<OpenGLCommandStream>,
                        [| typeof<obj> |],
                        true
                    )
                let il = meth.GetILGenerator()
                il.Emit(OpCodes.Ldarg, 0)
                il.Emit(OpCodes.Newobj, c)
                il.Emit(OpCodes.Ret)
                let del = meth.CreateDelegate(typeof<CommandStreamCreator>) |> unbox<CommandStreamCreator>
                Some del.Invoke
            with _ ->
                None
        else
            None


type internal CommandQueue<'a>(threads : int) =
    let lockObj = obj()

    let allThreads = Seq.init threads id |> Set.ofSeq
    let cancel = new CancellationTokenSource()
    let ct = cancel.Token
    let all = System.Collections.Generic.Queue<ref<Set<int>> * 'a>()
    let any = System.Collections.Generic.Queue<'a>()
    let mutable finished = false
    let reg = ct.Register (fun () -> lock lockObj (fun () -> Monitor.PulseAll lockObj))

    member x.Take(id : int) =
        lock lockObj (fun () ->
            let inline checkAll() = all.Count > 0 && Set.contains id (!fst(all.Peek()))
            let inline checkAny() = any.Count > 0

            let mutable hasAll = checkAll()
            let mutable hasAny = checkAny()

            while not hasAll && not hasAny && not ct.IsCancellationRequested && not finished  do
                Monitor.Wait lockObj |> ignore
                hasAll <- checkAll()
                hasAny <- checkAny()

            if ct.IsCancellationRequested then
                None
            elif hasAll then
                let set, action = all.Peek()
                set := Set.remove id !set
                if Set.isEmpty !set then
                    all.Dequeue() |> ignore
                Some action
            elif hasAny then
                let value = any.Dequeue()
                Some value
            else
                None
        )
        
    member x.AddAny(value : 'a) =
        lock lockObj (fun () ->
            if finished then raise <| ObjectDisposedException("CommandQueue")
            any.Enqueue value
            Monitor.PulseAll lockObj
        )

    member x.AddAll(value : 'a) =
        lock lockObj (fun () ->
            if finished then raise <| ObjectDisposedException("CommandQueue")
            all.Enqueue(ref allThreads, value)
            Monitor.PulseAll lockObj
        )
    member x.AddSpecific(threads : seq<int>, value : 'a) =
        lock lockObj (fun () ->
            if finished then raise <| ObjectDisposedException("CommandQueue")
            all.Enqueue(ref (Set.ofSeq threads), value)
            Monitor.PulseAll lockObj
        )

    member x.Cancel() =
        lock lockObj (fun () ->
            finished <- true
            cancel.Cancel()
        )

    member x.Completed() =
        lock lockObj (fun () ->
            finished <- true
            Monitor.PulseAll lockObj
        )

    member x.Dispose() =
        reg.Dispose()
        all.Clear()
        any.Clear()
        cancel.Dispose()
        
    interface IDisposable with
        member x.Dispose() = x.Dispose()

[<RequireQualifiedAccess>]
type OpenGLDebugMessage =
    | Info of message : string
    | Performance of message : string
    | Warning of message : string
    | Error of message : string

type OpenGLDevice(cfg : DeviceConfig) =
    inherit Device()

    do if cfg.nVidia then DynamicLinker.tryLoadLibrary ("nvapi64" + libraryExtension) |> ignore

    static let toBufferStorageMask (usage : BufferUsage) =
        let mutable res = BufferStorageMask.MapReadBit ||| BufferStorageMask.MapWriteBit
        if usage.HasFlag BufferUsage.CopyDst then res <- res ||| BufferStorageMask.DynamicStorageBit
        uint32 res

    static let deviceThread (queue : CommandQueue<GL -> unit>) (ctx : ContextHandle) (gl : GL) (id : int) () =
        ctx.MakeCurrent()
        try
            let mutable current = queue.Take(id)
            while Option.isSome current do
                try current.Value gl
                with _ -> ()
                current <- queue.Take(id)
        finally 
            ctx.ReleaseCurrent()

    static let createCommandStream =
        Introspection.GetAllClassesInheritingFrom(typeof<OpenGLDevice>.Assembly, typeof<OpenGLCommandStream>)
        |> Seq.toArray
        |> Array.map (fun t ->
            let arr = t.GetCustomAttributes(typeof<CommandStreamScoreAttribute>, true)
            let score = 
                if arr.Length > 0 then
                    arr |> Array.map (fun a -> (unbox<CommandStreamScoreAttribute> a).Score) |> Array.max
                else
                    0
            t, score
        )
        |> Array.sortByDescending snd
        |> Array.pick (fun (t,_) ->
            let ctor = 
                t.GetConstructor(
                    Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance,
                    Type.DefaultBinder,
                    [| typeof<OpenGLDevice> |],
                    null
                )



            if isNull ctor then
                Log.warn "bad commandstream type %A (no constructor)" t
                None
            else    
                match Reflection.ctor ctor with
                | Some ctor ->
                    Log.line "using %s" t.Name
                    Some ctor
                | None ->
                    None

        )

    let contexts =
        let res = Array.zeroCreate (max cfg.queues 1)
        let mutable last = None
        for i in 0 .. res.Length - 1 do
            let c = ContextHandle.Create(cfg.features.version, debug = cfg.debug, ?sharedWith = last)
            res.[i] <- c
            last <- Some c
        res
        
    let gl = GL.GetApi()
    let useContext (action : unit -> 'a) =
        contexts.[0].MakeCurrent()
        try action()
        finally contexts.[0].ReleaseCurrent()


    let enabledExtensions =
        HashSet.ofList [
            if cfg.features.directState then yield typeof<ArbDirectStateAccess>
            if cfg.features.copyBuffer then yield typeof<ArbCopyBuffer>
            if cfg.features.copyImage then yield typeof<ArbCopyImage>
            if cfg.features.bufferStorage then yield typeof<ArbBufferStorage>
            if cfg.features.textureStorage then 
                yield typeof<ArbTextureStorage>
                yield typeof<ArbTextureStorageMultisample>
        ]

    let functionTable, extensions =
        useContext (fun () ->
            let ctx = ContextHandle.Current.Value

            let extensions = Dict<Type, obj>()
            let table = Dict<string, nativeint>()
            let all = typeof<GL>.GetMethods(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)
            for a in all do
                let atts = a.GetCustomAttributes(typeof<Ultz.SuperInvoke.NativeApiAttribute>, true)
                if atts.Length > 0 then
                    for a in atts do
                        let a = unbox<Ultz.SuperInvoke.NativeApiAttribute> a
                        let ptr = ctx.GetProcAddress a.EntryPoint
                        if ptr <> 0n then
                            table.[a.EntryPoint] <- ptr

            let extTypes = 
                typeof<ArbCopyBuffer>.Assembly.GetTypes()
                |> Array.filter (fun t -> typeof<NativeExtension<GL>>.IsAssignableFrom t)

            let getExt = typeof<GL>.GetMethod "TryGetExtension"
            for e in extTypes do
                if enabledExtensions.Contains e then
                    let m = getExt.MakeGenericMethod [| e |]
                    let arr = [| null |]
                    let res = m.Invoke(gl, arr) |> unbox<bool>
                    if res then
                        extensions.[e] <- arr.[0]
                        let all = e.GetMethods(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)
                        for a in all do
                            let atts = a.GetCustomAttributes(typeof<Ultz.SuperInvoke.NativeApiAttribute>, true)
                            if atts.Length > 0 then
                                for a in atts do
                                    let a = unbox<Ultz.SuperInvoke.NativeApiAttribute> a
                                    let ptr = ctx.GetProcAddress a.EntryPoint
                                    if ptr <> 0n then
                                        table.[a.EntryPoint] <- ptr

            table, extensions
        )

    let info =
        useContext (fun () ->
            DeviceInfo.read gl cfg
        )

    let directState =
        match extensions.TryGetValue(typeof<ArbDirectStateAccess>) with
        | true, (:? ArbDirectStateAccess as e) -> Some e
        | _ -> None
            
    let bufferStorage =
        match extensions.TryGetValue(typeof<ArbBufferStorage>) with
        | true, (:? ArbBufferStorage as e) -> Some e
        | _ -> None
            
    let copyBuffer =
        match extensions.TryGetValue(typeof<ArbCopyBuffer>) with
        | true, (:? ArbCopyBuffer as e) -> Some e
        | _ -> None
        
    let copyImage =
        match extensions.TryGetValue(typeof<ArbCopyImage>) with
        | true, (:? ArbCopyImage as e) -> Some e
        | _ -> None
        
    let textureStorage =
        match extensions.TryGetValue(typeof<ArbTextureStorage>) with
        | true, (:? ArbTextureStorage as e) -> Some e
        | _ -> None

    let textureStorageMS =
        match extensions.TryGetValue(typeof<ArbTextureStorageMultisample>) with
        | true, (:? ArbTextureStorageMultisample as e) -> Some e
        | _ -> None
        
        


    let queue = new CommandQueue<GL -> unit>(contexts.Length)
    let threads =
        contexts |> Array.mapi (fun id ctx ->
            let thread = 
                Thread(
                    ThreadStart(deviceThread queue ctx gl id),
                    IsBackground = true,
                    Name = sprintf "OpenGL[%d]" id,
                    Priority = ThreadPriority.Highest
                )
            thread.Start()
            thread
        )

    let runAll (action : ContextHandle -> GL -> unit) =
        let missing = ref contexts.Length
        let exceptions = System.Collections.Generic.List<exn>()
        let action gl =
            try
                let current = ContextHandle.Current.Value
                action current gl
                gl.Flush()
                gl.Finish()
                lock missing (fun () -> 
                    missing := !missing - 1
                    if !missing = 0 then
                        Monitor.PulseAll missing
                )
            with e ->
                lock missing (fun () ->
                    exceptions.Add e
                    missing := !missing - 1
                    if !missing = 0 then
                        Monitor.PulseAll missing
                )
        queue.AddAll action
        lock missing (fun () ->
            while !missing > 0 do
                Monitor.Wait missing |> ignore
        )

        if exceptions.Count > 0 then
            raise <| AggregateException(exceptions)
        else
            ()

    let debugMessages =
        if cfg.debug then
            let all = System.Collections.Generic.HashSet<IObserver<OpenGLDebugMessage>>()

            runAll (fun _ gl ->
                let proc = 
                    DebugProc (fun source typ id severity length message _ -> 
                        let str = Marshal.PtrToStringAnsi(message, length)
                        let message =
                            match unbox<DebugType> (int typ) with
                            | DebugType.DebugTypeError -> 
                                match unbox<DebugSeverity> (int severity) with
                                | DebugSeverity.DebugSeverityHigh -> OpenGLDebugMessage.Error str
                                | _ -> OpenGLDebugMessage.Warning str
                            | DebugType.DebugTypePerformance -> 
                                OpenGLDebugMessage.Performance str
                            | _ ->  
                                OpenGLDebugMessage.Info str

                        lock all (fun () ->
                            for a in all do a.OnNext message
                        )
                    )
                lock gl (fun () ->
                    gl.DebugMessageCallback(proc, VoidPtr.zero)   
                )
            )

            { new IObservable<OpenGLDebugMessage> with
                member x.Subscribe(obs : IObserver<OpenGLDebugMessage>) =
                    lock all (fun () ->
                        if all.Add obs then
                            if all.Count = 1 then
                                runAll (fun _ gl -> 
                                    gl.Enable(EnableCap.DebugOutput)
                                    gl.Enable(EnableCap.DebugOutputSynchronous)
                                )

                            { new IDisposable with
                                member x.Dispose() =
                                    lock all (fun () -> 
                                        if all.Remove obs then
                                            if all.Count = 0 then
                                                runAll (fun _ gl -> 
                                                    gl.Disable(EnableCap.DebugOutput)
                                                    gl.Disable(EnableCap.DebugOutputSynchronous)
                                                )
                                            
                                    )
                            }
                        else
                            { new IDisposable with
                                member x.Dispose() = ()
                            }
                    )
            }
        else
            { new IObservable<OpenGLDebugMessage> with
                member x.Subscribe _ = { new IDisposable with member x.Dispose() = () }
            }

    let mutable debugSeverity = if cfg.debug then 4 else 0
    let mutable debugOuputSubscription : option<IDisposable> = None

    let setDebugReport (v : bool) =
        if cfg.debug then
            match debugOuputSubscription with
            | Some s when not v -> 
                s.Dispose()
                debugOuputSubscription <- None
            | None when v -> 
                let sub =
                    debugMessages.Subscribe (fun v ->
                        match v with
                        | OpenGLDebugMessage.Info msg -> Report.Line("[GL] {0}", msg)
                        | OpenGLDebugMessage.Warning msg -> Report.WarnNoPrefix("[GL] {0}", msg)
                        | OpenGLDebugMessage.Error msg -> Report.ErrorNoPrefix("[GL] {0}", msg)
                        | OpenGLDebugMessage.Performance msg -> Report.Line("[GL] {0}", msg)
                    )
                debugOuputSubscription <- Some sub
            | _ ->
                ()

    let setDebugSeverity (value : int) =
        if cfg.debug then
            runAll (fun _ gl ->
                if value < 4 then
                    gl.DebugMessageControl(DebugSource.DontCare, DebugType.DontCare, DebugSeverity.DontCare, 0u, NativePtr.zero, false)
                    gl.DebugMessageControl(DebugSource.DontCare, DebugType.DebugTypeError, DebugSeverity.DebugSeverityHigh, 0u, NativePtr.zero, true)

                    if value >= 1 then
                        gl.DebugMessageControl(DebugSource.DontCare, DebugType.DebugTypeError, DebugSeverity.DontCare, 0u, NativePtr.zero, true)
                    if value >= 2 then
                        gl.DebugMessageControl(DebugSource.DontCare, DebugType.DebugTypeUndefinedBehavior, DebugSeverity.DontCare, 0u, NativePtr.zero, true)
                        gl.DebugMessageControl(DebugSource.DontCare, DebugType.DebugTypeDeprecatedBehavior, DebugSeverity.DontCare, 0u, NativePtr.zero, true)
                    if value >= 3 then
                        gl.DebugMessageControl(DebugSource.DontCare, DebugType.DebugTypePerformance, DebugSeverity.DontCare, 0u, NativePtr.zero, true)
                else
                    gl.DebugMessageControl(DebugSource.DontCare, DebugType.DontCare, DebugSeverity.DontCare, 0u, NativePtr.zero, true)
                
            )
            debugSeverity <- value

    do setDebugSeverity debugSeverity
       setDebugReport cfg.debug

    static let freeBuffer (this : OpenGLDevice) (handle : obj) =
        this.Start(fun gl ->
            gl.DeleteBuffer (unbox<uint32> handle)
        )
    static let freeImage (this : OpenGLDevice) (handle : obj) =
        this.Start(fun gl ->
            gl.DeleteTexture (unbox<uint32> handle)
        )

    member x.DirectState = directState
    member x.BufferStorage = bufferStorage
    member x.CopyBuffer = copyBuffer
    member x.CopyImage = copyImage

    member x.Info = info
    member x.Features = info.features


    override x.Name = info.renderer

    override x.Dispose() =
        queue.Cancel()
        for t in threads do t.Join()
        queue.Dispose()
        queue.Dispose()

    member x.Start(action : GL -> unit) =
        queue.AddAny (fun gl ->
            action gl
            gl.Flush()
            gl.Finish()
        )

    member x.StartAsTask(action : GL -> 'a) =
        let tcs = TaskCompletionSource<'a>()
        let action gl =
            try
                let res = action gl
                gl.Flush()
                gl.Finish()
                tcs.SetResult res
            with e ->
                tcs.SetException e
        queue.AddAny action
        tcs.Task

    member x.Run(action : GL -> 'a) =
        let res : ref<option<Choice<'a, exn>>> = ref None
        let action gl =
            try
                let value = action gl
                gl.Flush()
                gl.Finish()
                lock res (fun () -> 
                    res := Some (Choice1Of2 value)
                    Monitor.PulseAll res
                )
            with e ->
                lock res (fun () ->
                    res := Some (Choice2Of2 e)
                    Monitor.PulseAll res

                )
        queue.AddAny action
        lock res (fun () ->
            while Option.isNone !res do
                Monitor.Wait res |> ignore
        )
        match res.Value.Value with
        | Choice1Of2 v -> v
        | Choice2Of2 e -> raise e

    
    member x.DebugMessages = debugMessages

    member x.DebugSeverity 
        with get() = debugSeverity
        and set v = if v <> debugSeverity then setDebugSeverity v

    member x.DebugReport
        with get() = Option.isSome debugOuputSubscription
        and set v = setDebugReport v

    member internal x.GetProcAddress(name : string) : nativeint =
        match functionTable.TryGetValue name with
        | (true, ptr) -> ptr
        | _ -> 0n

    override x.Start(cmd : CommandStream) =
        match cmd with
        | :? OpenGLCommandStream as cmd -> x.Start (fun gl -> cmd.Run(ContextHandle.Current.Value, gl))
        | _ -> failwithf "[GL] bad command stream %A" cmd
        
    override x.StartAsTask(cmd : CommandStream) =
        match cmd with
        | :? OpenGLCommandStream as cmd -> x.StartAsTask (fun gl -> cmd.Run(ContextHandle.Current.Value, gl)) :> Task
        | _ -> failwithf "[GL] bad command stream %A" cmd
        
    override x.Run(cmd : CommandStream) =
        match cmd with
        | :? OpenGLCommandStream as cmd -> x.Run (fun gl -> cmd.Run(ContextHandle.Current.Value, gl))
        | _ -> failwithf "[GL] bad command stream %A" cmd

    override x.CreateBuffer(size : int64, usage : BufferUsage) =
        x.Run (fun gl ->
            match directState with
            | Some ext ->
                let handle = ext.CreateBuffer()
                ext.NamedBufferStorage(handle, uint32 size, VoidPtr.zero, toBufferStorageMask usage)
                new Anteater.Buffer(handle, size, usage, freeBuffer x)
            | None -> 
                let handle = gl.GenBuffer()
                match bufferStorage with
                | Some ext ->
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, handle)
                    ext.BufferStorage(BufferStorageTarget.ArrayBuffer, uint32 size, VoidPtr.zero, toBufferStorageMask usage)
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, 0u)

                | None -> 
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, handle)
                    gl.BufferData(BufferTargetARB.ArrayBuffer, uint32 size, VoidPtr.zero, BufferUsageARB.StaticDraw)
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, 0u)

                new Anteater.Buffer(handle, size, usage, freeBuffer x)
        )



    member x.CreateImage(dim : ImageDimension, format : ImageFormat, ?levels : int, ?slices : int, ?samples : int) : Image =
        let isArray = Option.isSome slices
        let levels = defaultArg levels 1
        let samples = defaultArg samples 1
        let slices = defaultArg slices 1
        let fmt = unbox<InternalFormat> (int format)

        match dim with
        | ImageDimension.ImageCube _ when samples > 1 -> failwith "[GL] multisampled cubemaps are not supported (please use arrays)"
        | _ -> ()

        let info = dim.GetTarget(samples, isArray)

        x.Run (fun gl ->
            match directState with
            | Some ext ->
                let storeSize = dim.GetStorageSize slices
                let arr = [| 0u |]
                ext.CreateTextures(info.target, 1u, Span arr)
                let handle = arr.[0]

                match info.storeDimension, info.multisampled with
                | 1, false -> 
                    ext.TextureStorage1D(handle, uint32 levels, fmt, uint32 storeSize.X)
                | 2, false ->
                    ext.TextureStorage2D(handle, uint32 levels, fmt, uint32 storeSize.X, uint32 storeSize.Y)
                | 2, true ->
                    ext.TextureStorage2DMultisample(handle, uint32 samples, fmt, uint32 storeSize.X, uint32 storeSize.Y, false)
                | 3, false ->
                    ext.TextureStorage3D(handle, uint32 levels, fmt, uint32 storeSize.X, uint32 storeSize.Y, uint32 storeSize.Z)
                | 3, true ->
                    ext.TextureStorage3DMultisample(handle, uint32 samples, fmt, uint32 storeSize.X, uint32 storeSize.Y, uint32 storeSize.Z, false)
                | dim, ms ->
                    failwithf "[GL] cannot allocate image with dimension %d and ms %A" dim ms


                new Image(handle, dim, format, levels, (if isArray then Some slices else None), samples, freeImage x)
            | None ->
                let handle = gl.GenTexture()
                gl.BindTexture(info.target, handle)
                    
                if info.multisampled then
                    match textureStorageMS with
                    | Some ext ->
                        let storeSize = dim.GetStorageSize slices
                        match info.storeDimension with
                        | 2 -> ext.TexStorage2DMultisample(info.target, uint32 samples, fmt, uint32 storeSize.X, uint32 storeSize.Y, false)
                        | 3 -> ext.TexStorage3DMultisample(info.target, uint32 samples, fmt, uint32 storeSize.X, uint32 storeSize.Y, uint32 storeSize.Z, false)
                        | dim -> failwithf "[GL] cannot allocate multisampled image with dimension %d" dim
                    | None ->
                        let storeSize = dim.GetImageSize slices

                        
                        let mutable baseLevel = 0
                        let mutable maxLevel = 0
                        gl.TexParameterI(info.target, TextureParameterName.TextureBaseLevel, &baseLevel)
                        gl.TexParameterI(info.target, TextureParameterName.TextureMaxLevel, &maxLevel)

                        match info.imageDimension with
                        | 2 -> 
                            for target in info.targets do
                                gl.TexImage2DMultisample(target, uint32 samples, fmt, uint32 storeSize.X, uint32 storeSize.Y, false)
                        | 3 -> 
                            for target in info.targets do
                                gl.TexImage3DMultisample(target, uint32 samples, fmt, uint32 storeSize.X, uint32 storeSize.Y, uint32 storeSize.Z, false)
                        | dim -> 
                            failwithf "[GL] cannot allocate multisampled image with dimension %d" dim
                    
                else
                    match textureStorage with
                    | Some ext ->
                        let storeSize = dim.GetStorageSize slices
                        match info.storeDimension with
                        | 1 -> ext.TexStorage1D(info.target, uint32 levels, fmt, uint32 storeSize.X)
                        | 2 -> ext.TexStorage2D(info.target, uint32 levels, fmt, uint32 storeSize.X, uint32 storeSize.Y)
                        | 3 -> ext.TexStorage3D(info.target, uint32 levels, fmt, uint32 storeSize.X, uint32 storeSize.Y, uint32 storeSize.Z)
                        | dim -> failwithf "[GL] cannot allocate image with dimension %d" dim
                    | None ->
                        let mutable baseLevel = 0
                        let mutable maxLevel = levels - 1
                        gl.TexParameterI(info.target, TextureParameterName.TextureBaseLevel, &baseLevel)
                        gl.TexParameterI(info.target, TextureParameterName.TextureMaxLevel, &maxLevel)

                        let t = PixelType.Float
                        let f = PixelFormat.Rgba

                        match info.imageDimension with
                        | 1 ->
                            let mutable dim = dim
                            for l in 0 .. levels - 1 do
                                let storeSize = dim.GetImageSize slices
                                for target in info.targets do
                                    gl.TexImage1D(target, l, int fmt, uint32 storeSize.X, 0, f, t, VoidPtr.zero)
                                dim <- dim / 2
                        | 2 ->
                            let mutable dim = dim
                            for l in 0 .. levels - 1 do
                                let storeSize = dim.GetImageSize slices
                                for target in info.targets do
                                    gl.TexImage2D(target, l, int fmt, uint32 storeSize.X, uint32 storeSize.Y, 0, f, t, VoidPtr.zero)
                                dim <- dim / 2
                        | 3 ->
                            let mutable dim = dim
                            for l in 0 .. levels - 1 do
                                let storeSize = dim.GetImageSize slices
                                for target in info.targets do
                                    gl.TexImage3D(target, l, int fmt, uint32 storeSize.X, uint32 storeSize.Y, uint32 storeSize.Z, 0, f, t, VoidPtr.zero)
                                dim <- dim / 2
                        | dim -> 
                            failwithf "[GL] cannot allocate image with dimension %d" dim

                gl.BindTexture(info.target, 0u)
                new Image(handle, dim, format, levels, (if isArray then Some slices else None), samples, freeImage x)
        )

           

    override x.CreateCommandStream() =
        createCommandStream x :> CommandStream


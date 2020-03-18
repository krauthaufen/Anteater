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
        bufferStorage   : bool
    }

    member private x.AsString = x.ToString()
    override x.ToString() =
        let exts =
            [
                if x.directState then yield "DirectStateAccess"
                if x.copyBuffer then yield "CopyBuffer"
                if x.bufferStorage then yield "BufferStorage"
            ]

        match exts with
        | [] -> sprintf "OpenGL %A" x.version
        | _ -> String.concat "; " exts |> sprintf "OpenGL %A { %s }" x.version

    
    static member None =
        {
            version         = Version(4,1)
            directState     = false
            copyBuffer      = false
            bufferStorage   = false
        }

    static member Default =
        {
            version         = Version(4,1)
            directState     = true
            copyBuffer      = true
            bufferStorage   = true
        }

        

type DeviceConfig =
    {
        nVidia      : bool
        queues      : int
        features    : OpenGLFeatures
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

module DeviceInfo =
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
                    bufferStorage = cfg.features.bufferStorage && fst (gl.TryGetExtension<ArbBufferStorage>())
                }
        }


type OpenGLDevice(cfg : DeviceConfig) =
    inherit Device()

    do if cfg.nVidia then DynamicLinker.tryLoadLibrary ("nvapi64" + libraryExtension) |> ignore

    
    static let freeBuffer (this : OpenGLDevice) (handle : obj) =
        this.Start(fun gl ->
            gl.DeleteBuffer (unbox<uint32> handle)
        )
        
    static let toBufferStorageMask (usage : BufferUsage) =
        let mutable res = BufferStorageMask.MapReadBit ||| BufferStorageMask.MapWriteBit
        if usage.HasFlag BufferUsage.CopyDst then res <- res ||| BufferStorageMask.DynamicStorageBit
        uint32 res

    static let deviceThread (ct : CancellationToken) (queue : BlockingCollection<GL -> unit>) (ctx : ContextHandle) (gl : GL) () =
        ctx.MakeCurrent()
        try
            try
                for e in queue.GetConsumingEnumerable(ct) do
                    try e gl
                    with _ -> ()
            with :? OperationCanceledException ->
                ()
        finally 
            ctx.ReleaseCurrent()

    let contexts =
        let res = Array.zeroCreate (max cfg.queues 1)
        let mutable last = None
        for i in 0 .. res.Length - 1 do
            let c = ContextHandle.Create(cfg.features.version, ?sharedWith = last)
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
            if cfg.features.bufferStorage then yield typeof<ArbBufferStorage>
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
        | true, (:? ArbBufferStorage as e) -> Some e
        | _ -> None
        
        


    let cancel = new CancellationTokenSource()
    let queue = new BlockingCollection<GL -> unit>()
    let threads =
        contexts |> Array.mapi (fun id ctx ->
            let thread = 
                Thread(
                    ThreadStart(deviceThread cancel.Token queue ctx gl),
                    IsBackground = true,
                    Name = sprintf "OpenGL[%d]" id,
                    Priority = ThreadPriority.Highest
                )
            thread.Start()
            thread
        )

    member x.DirectState = directState
    member x.BufferStorage = bufferStorage
    member x.CopyBuffer = copyBuffer

    member x.Info = info
    member x.Features = info.features

    override x.Name = info.renderer

    override x.Dispose() =
        cancel.Cancel()
        for t in threads do t.Join()
        queue.Dispose()
        cancel.Dispose()

    member x.Start(action : GL -> unit) =
        queue.Add (fun gl ->
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
        queue.Add action
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
        queue.Add action
        lock res (fun () ->
            while Option.isNone !res do
                Monitor.Wait res |> ignore
        )
        match res.Value.Value with
        | Choice1Of2 v -> v
        | Choice2Of2 e -> raise e
 
    member internal x.GetProcAddress(name : string) : nativeint =
        match functionTable.TryGetValue name with
        | (true, ptr) -> ptr
        | _ -> 0n

    override x.Start(cmd : CommandStream) =
        match cmd with
        | :? ManagedOpenGLCommandStream as cmd -> x.Start cmd.Run
        | :? NativeGLCommandStream as cmd -> x.Start cmd.Run
        | _ -> failwithf "[GL] bad command stream %A" cmd
        
    override x.StartAsTask(cmd : CommandStream) =
        match cmd with
        | :? ManagedOpenGLCommandStream as cmd -> x.StartAsTask cmd.Run :> Task
        | :? NativeGLCommandStream as cmd -> x.StartAsTask cmd.Run :> Task
        | _ -> failwithf "[GL] bad command stream %A" cmd
        
    override x.Run(cmd : CommandStream) =
        match cmd with
        | :? ManagedOpenGLCommandStream as cmd -> x.Run cmd.Run
        | :? NativeGLCommandStream as cmd -> x.Run cmd.Run
        | _ -> failwithf "[GL] bad command stream %A" cmd

    override x.CreateBuffer(size : int64, usage : BufferUsage) =
        x.Run (fun gl ->
            let handle = gl.GenBuffer()
            match directState with
            | Some ext ->
                ext.NamedBufferStorage(handle, uint32 size, VoidPtr.zero, toBufferStorageMask usage)
            | None -> 
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

    override x.CreateCommandStream() =
        //new ManagedOpenGLCommandStream(x) :> CommandStream
        if RuntimeInformation.ProcessArchitecture = Architecture.X64 then new NativeGLCommandStream(x) :> CommandStream
        else new ManagedOpenGLCommandStream(x) :> CommandStream

and ManagedOpenGLCommandStream(device : OpenGLDevice) =
    inherit CommandStream()

    let actions = System.Collections.Generic.List<GL -> unit>()

    override x.Dispose(_) =
        actions.Clear()

    override x.Copy(src : BufferRange, dst : BufferRange) =
        let size = min src.Size dst.Size
        let srcOffset = src.Offset
        let dstOffset = dst.Offset
        if size > 0L then
            let sh = unbox<uint32> src.Buffer.Handle
            let dh = unbox<uint32> dst.Buffer.Handle
            actions.Add <| fun gl ->
                match gl.TryGetExtension<ArbDirectStateAccess>() with
                | true, ext ->
                    ext.CopyNamedBufferSubData(sh, dh, int srcOffset, int dstOffset, uint32 size)
                | _ ->
                    match gl.TryGetExtension<ArbCopyBuffer>() with
                    | true, ext ->
                        gl.BindBuffer(BufferTargetARB.CopyReadBuffer, sh)
                        gl.BindBuffer(BufferTargetARB.CopyWriteBuffer, dh)
                        ext.CopyBufferSubData(CopyBufferSubDataTarget.CopyReadBuffer, CopyBufferSubDataTarget.CopyWriteBuffer, int srcOffset, int dstOffset, uint32 size)
                        gl.BindBuffer(BufferTargetARB.CopyReadBuffer, 0u)
                        gl.BindBuffer(BufferTargetARB.CopyWriteBuffer, 0u)
                    | _ ->
                        gl.BindBuffer(BufferTargetARB.ArrayBuffer, sh)
                        gl.BindBuffer(BufferTargetARB.ElementArrayBuffer, dh)
                        let psrc = gl.MapBufferRange(BufferTargetARB.ArrayBuffer, nativeint srcOffset, unativeint size, uint32 BufferAccessARB.ReadOnly) |> VoidPtr.toNativeInt
                        let pdst = gl.MapBufferRange(BufferTargetARB.ElementArrayBuffer, nativeint dstOffset, unativeint size, uint32  BufferAccessARB.WriteOnly) |> VoidPtr.toNativeInt
                        Marshal.Copy(psrc, pdst, size)
                        gl.UnmapBuffer BufferTargetARB.ArrayBuffer |> ignore
                        gl.UnmapBuffer BufferTargetARB.ElementArrayBuffer |> ignore

    override x.Copy(src : nativeint, dst : BufferRange) =
        let size = dst.Size
        if size > 0L then
            let dstOffset = dst.Offset
            let dh = unbox<uint32> dst.Buffer.Handle
            match device.DirectState with
            | Some ext ->
                actions.Add <| fun gl ->
                    let pdst = ext.MapNamedBufferRange(dh, int dstOffset, uint32 size, uint32 (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit)) |> VoidPtr.toNativeInt
                    Marshal.Copy(src, pdst, size)
                    ext.UnmapNamedBuffer(dh) |> ignore
            | None -> 
                actions.Add <| fun gl ->
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, dh)
                    let pdst = gl.MapBufferRange(BufferTargetARB.ArrayBuffer, nativeint dstOffset, unativeint size, uint32 (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit)) |> VoidPtr.toNativeInt
                    Marshal.Copy(src, pdst, size)
                    gl.UnmapBuffer BufferTargetARB.ArrayBuffer |> ignore
                
    override x.Copy(src : BufferRange, dst : nativeint) =
        let size = src.Size
        if size > 0L then
            let sh = unbox<uint32> src.Buffer.Handle
            actions.Add <| fun gl ->
                match gl.TryGetExtension<ArbDirectStateAccess>() with
                | true, ext ->
                    let psrc = ext.MapNamedBufferRange(sh, int src.Offset, uint32 size, uint32 MapBufferAccessMask.MapReadBit) |> VoidPtr.toNativeInt
                    Marshal.Copy(psrc, dst, size)
                    ext.UnmapNamedBuffer(sh) |> ignore
                | _ ->
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, sh)
                    let psrc = gl.MapBufferRange(BufferTargetARB.ArrayBuffer, nativeint src.Offset, unativeint size, uint32 MapBufferAccessMask.MapReadBit) |> VoidPtr.toNativeInt
                    Marshal.Copy(psrc, dst, size)
                    gl.UnmapBuffer BufferTargetARB.ArrayBuffer |> ignore

    override x.Copy<'T when 'T : unmanaged>(src : Memory<'T>, dst : Anteater.BufferRange) =
        let handle = src.Pin()

        let srcSize = int64 src.Length * int64 sizeof<'T>
        if srcSize < dst.Size then
            x.Copy(VoidPtr.toNativeInt handle.Pointer, dst.[..srcSize-1L])
        else
            x.Copy(VoidPtr.toNativeInt handle.Pointer, dst)

        actions.Add (ignore >> handle.Dispose)
        
    override x.Copy<'T when 'T : unmanaged>(src : Anteater.BufferRange, dst : Memory<'T>) =
        let handle = dst.Pin()
        
        let dstSize = int64 dst.Length * int64 sizeof<'T>
        if dstSize < src.Size then
            x.Copy(src.[..dstSize-1L], VoidPtr.toNativeInt handle.Pointer)
        else
            x.Copy(src, VoidPtr.toNativeInt handle.Pointer)
        actions.Add (ignore >> handle.Dispose)

    member internal x.Run(gl : GL) =
        for a in actions do a gl

and NativeGLCommandStream(device : OpenGLDevice) =
    inherit CommandStream()
    
    static let memcpy = 
        if isWindows then 
            let lib = DynamicLinker.loadLibrary("msvcrt.dll")
            DynamicLinker.tryLoadFunction "memcpy" lib
        else
            let lib = DynamicLinker.loadLibrary("libc")
            DynamicLinker.tryLoadFunction "memcpy" lib

    let memory = new ExecutableStream()
    let assembler = AssemblerStream.ofStream memory
    let mutable retOffset = 0L
    let mutable wrapped : Option<nativeint * ExecutableStream * (unit -> unit)> = None

    let preamble = System.Collections.Generic.List<nativeint * (unit -> (unit -> unit))>()

    let alloc (create : unit -> 'a * (unit -> unit)) =
        let ptr = Marshal.AllocHGlobal sizeof<'a>
        preamble.Add(ptr, fun () ->
            let v, release = create()
            NativeInt.write ptr v
            release
        )
        ptr

    member private x.Copy(src : nativeint, dst : BufferRange, srcIndirect : bool) =
        let size = dst.Size
        if size > 0L then
            memory.Position <- retOffset

            let dstOffset = dst.Offset
            let dh = unbox<uint32> dst.Buffer.Handle
            let mapDirect = device.GetProcAddress "glMapNamedBufferRange"
            let unmapDirect = device.GetProcAddress "glUnmapNamedBuffer"
            let bind = device.GetProcAddress "glBindBuffer"
            let map = device.GetProcAddress "glMapBufferRange"
            let unmap = device.GetProcAddress "glUnmapBuffer"

            if mapDirect <> 0n then
                // map
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (int dh)
                assembler.Call mapDirect
                assembler.Mov(assembler.CalleeSavedRegisters.[0], assembler.ReturnRegister)

                // memcpy
                assembler.BeginCall(3)
                assembler.PushArg(nativeint size)
                if srcIndirect then assembler.PushPtrArg src
                else assembler.PushArg src
                assembler.PushArg src
                assembler.Mov(assembler.ArgumentRegisters.[0], assembler.CalleeSavedRegisters.[0])
                assembler.Call(memcpy.Value.Handle)

                // unmap
                assembler.BeginCall(1)
                assembler.PushArg (int dh)
                assembler.Call(unmapDirect)

            else
                // bind
                assembler.BeginCall(2)
                assembler.PushArg(int dh)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                
                // map
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call map
                assembler.Mov(assembler.CalleeSavedRegisters.[0], assembler.ReturnRegister)
                
                // memcpy
                assembler.BeginCall(3)
                assembler.PushArg(nativeint size)
                if srcIndirect then assembler.PushPtrArg src
                else assembler.PushArg src
                assembler.PushArg 0n
                assembler.Mov(assembler.ArgumentRegisters.[0], assembler.CalleeSavedRegisters.[0])
                assembler.Call(memcpy.Value.Handle)
                
                // unmap
                assembler.BeginCall(1)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call(unmap)


                // unbind
                assembler.BeginCall(2)
                assembler.PushArg 0
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                
            retOffset <- memory.Position
            assembler.Ret()
        
    member private x.Copy(src : BufferRange, dst : nativeint, dstIndirect : bool) =
        let size = src.Size
        if size > 0L then
            memory.Position <- retOffset

            let srcOffset = src.Offset
            let sh = unbox<uint32> src.Buffer.Handle
            let bind = device.GetProcAddress "glBindBuffer"
            let map = device.GetProcAddress "glMapBufferRange"
            let unmap = device.GetProcAddress "glUnmapBuffer"
            let mapDirect = device.GetProcAddress "glMapNamedBufferRange"
            let unmapDirect = device.GetProcAddress "glUnmapNamedBuffer"

            if mapDirect <> 0n then
                // map
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapReadBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int sh)
                assembler.Call mapDirect
                assembler.Mov(assembler.CalleeSavedRegisters.[0], assembler.ReturnRegister)

                // memcpy
                assembler.BeginCall(3)
                assembler.PushArg(nativeint size)
                assembler.PushArg dst
                assembler.Mov(assembler.ArgumentRegisters.[1], assembler.CalleeSavedRegisters.[0])
                if dstIndirect then assembler.PushPtrArg dst
                else assembler.PushArg dst
                assembler.Call(memcpy.Value.Handle)

                // unmap
                assembler.BeginCall(1)
                assembler.PushArg (int sh)
                assembler.Call(unmapDirect)

            else
                // bind
                assembler.BeginCall(2)
                assembler.PushArg(int sh)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                
                // map
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapReadBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call map
                assembler.Mov(assembler.CalleeSavedRegisters.[0], assembler.ReturnRegister)
                
                // memcpy
                assembler.BeginCall(3)
                assembler.PushArg(nativeint size)
                assembler.PushArg dst
                assembler.Mov(assembler.ArgumentRegisters.[1], assembler.CalleeSavedRegisters.[0])
                if dstIndirect then assembler.PushPtrArg dst
                else assembler.PushArg dst
                assembler.Call(memcpy.Value.Handle)
                
                // unmap
                assembler.BeginCall(1)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call(unmap)


                // unbind
                assembler.BeginCall(2)
                assembler.PushArg 0
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                
            retOffset <- memory.Position
            assembler.Ret()


    override x.Dispose(_) =
        memory.Dispose()
        for (ptr, _) in preamble do Marshal.FreeHGlobal ptr
        preamble.Clear()
        retOffset <- 0L
        match wrapped with
        | Some (_, e, _) -> 
            e.Dispose()
            wrapped <- None
        | _ ->
            ()

    override x.Copy(src : BufferRange, dst : BufferRange) =
        let size = min src.Size dst.Size
        let srcOffset = src.Offset
        let dstOffset = dst.Offset
        if size > 0L then
            memory.Position <- retOffset

            let sh = unbox<uint32> src.Buffer.Handle
            let dh = unbox<uint32> dst.Buffer.Handle
            let direct = device.GetProcAddress "glCopyNamedBufferSubData"
            let bind = device.GetProcAddress "glBindBuffer"
            let copy = device.GetProcAddress "glCopyBufferSubData"

            if direct <> 0n then
                assembler.BeginCall(5)
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int dh)
                assembler.PushArg (int sh)
                assembler.Call(direct)

            elif copy <> 0n then
                assembler.BeginCall(2)
                assembler.PushArg(int sh)
                assembler.PushArg(int BufferTargetARB.CopyReadBuffer)
                assembler.Call(bind)
                    
                assembler.BeginCall(2)
                assembler.PushArg(int dh)
                assembler.PushArg(int BufferTargetARB.CopyWriteBuffer)
                assembler.Call(bind)

                assembler.BeginCall(5)
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int BufferTargetARB.CopyWriteBuffer)
                assembler.PushArg (int BufferTargetARB.CopyReadBuffer)
                assembler.Call(copy)

                assembler.BeginCall(2)
                assembler.PushArg(0)
                assembler.PushArg(int BufferTargetARB.CopyReadBuffer)
                assembler.Call(bind)
                    
                assembler.BeginCall(2)
                assembler.PushArg(0)
                assembler.PushArg(int BufferTargetARB.CopyWriteBuffer)
                assembler.Call(bind)
            else    
                // mapped CPU copy

                let map = device.GetProcAddress "glMapBufferRange"
                let unmap = device.GetProcAddress "glUnmapBuffer"

                let srcPtr = assembler.CalleeSavedRegisters.[0]
                let dstPtr = assembler.CalleeSavedRegisters.[1]

                // srcPtr = map(src)
                assembler.BeginCall(2)
                assembler.PushArg(int sh)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                    
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapReadBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call map
                assembler.Mov(srcPtr, assembler.ReturnRegister)

                
                // dstPtr = map(dst)
                assembler.BeginCall(2)
                assembler.PushArg(int dh)
                assembler.PushArg(int BufferTargetARB.ElementArrayBuffer)
                assembler.Call(bind)
                
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (int BufferTargetARB.ElementArrayBuffer)
                assembler.Call map
                assembler.Mov(dstPtr, assembler.ReturnRegister)


                // copy
                assembler.BeginCall(3)
                assembler.PushArg (nativeint size)
                assembler.PushArg 0n
                assembler.PushArg 0n
                assembler.Mov(assembler.ArgumentRegisters.[0], dstPtr)
                assembler.Mov(assembler.ArgumentRegisters.[1], srcPtr)
                assembler.Call(memcpy.Value.Handle)

                // unmap
                assembler.BeginCall(1)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call unmap

                assembler.BeginCall(1)
                assembler.PushArg(int BufferTargetARB.ElementArrayBuffer)
                assembler.Call unmap

                
                assembler.BeginCall(2)
                assembler.PushArg(0)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)

                assembler.BeginCall(2)
                assembler.PushArg(0)
                assembler.PushArg(int BufferTargetARB.ElementArrayBuffer)
                assembler.Call(bind)

            retOffset <- memory.Position
            assembler.Ret()

    override x.Copy(src : nativeint, dst : BufferRange) =
        x.Copy(src, dst, false)

    override x.Copy(src : BufferRange, dst : nativeint) =
        x.Copy(src, dst, false)

    override x.Copy<'T when 'T : unmanaged>(src : Memory<'T>, dst : Anteater.BufferRange) =
        let handle = 
            alloc (fun () ->
                let p = src.Pin()
                VoidPtr.toNativeInt p.Pointer, p.Dispose
            )
        x.Copy(handle, dst, true)
        
    override x.Copy<'T when 'T : unmanaged>(src : Anteater.BufferRange, dst : Memory<'T>) =
        let handle = 
            alloc (fun () ->
                let p = dst.Pin()
                VoidPtr.toNativeInt p.Pointer, p.Dispose
            )
        x.Copy(src, handle, true)

    member x.Run(_gl : GL) =
        let run = 
            match wrapped with
            | Some (ptr, _, run) when ptr = memory.Pointer -> run
            | _ ->
                match wrapped with
                | Some (_, e, _) -> e.Dispose()
                | None -> ()

                let e = new ExecutableStream()
                let s = AssemblerStream.ofStream e
                for r in assembler.CalleeSavedRegisters do s.Push r
                s.BeginCall(0)
                s.Call(memory.Pointer)
                for r in assembler.CalleeSavedRegisters do s.Pop r
                s.Ret()

                let run : unit -> unit = UnmanagedFunctions.wrap e.Pointer
                wrapped <- Some (memory.Pointer, e, run)
                run

        let release = Array.zeroCreate preamble.Count
        let mutable i = 0
        for (_, p) in preamble do
            release.[i] <- p()
            i <- i + 1

        run()

        for r in release do r()
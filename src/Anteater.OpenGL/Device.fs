namespace Anteater.OpenGL

open System
open System.Threading
open System.Runtime.InteropServices
open System.Threading.Tasks
open System.Collections.Concurrent
open Aardvark.Base
open Silk.NET.OpenGL
open Aardvark.Base
open Microsoft.FSharp.NativeInterop
open Silk.NET.OpenGL.Extensions.ARB
open Anteater
open System.Runtime.CompilerServices

#nowarn "9"

type DeviceConfig =
    {
        version : Version
        nVidia : bool
        queues : int
    }


type DeviceInfo =
    {
        vendor      : string
        renderer    : string
        version     : Version
        glsl        : Version

        extensions  : Set<string>
    }

module DeviceInfo =
    open System.Text.RegularExpressions

    // major_number.minor_number major_number.minor_number.release_number

    let versionRx = Regex @"^([0-9]+)[ \t]*\.[ \t]*([0-9]+)([ \t]*\.[ \t]*([0-9]+))?"



    let read (gl : GL) =

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

        {
            vendor = gl.GetString(StringName.Vendor)
            renderer = gl.GetString(StringName.Renderer)
            version = Version(gl.GetInteger(GetPName.MajorVersion), gl.GetInteger(GetPName.MinorVersion))
            glsl = glsl

            extensions = Seq.init (gl.GetInteger GetPName.NumExtensions) (fun i -> gl.GetString(StringName.Extensions, uint32 i)) |> Set.ofSeq
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
        Array.init (max cfg.queues 1) (fun i ->
            ContextHandle.Create(cfg.version)
        )
        
    let gl = GL.GetApi()

    let useContext (action : unit -> 'a) =
        contexts.[0].MakeCurrent()
        try action()
        finally contexts.[0].ReleaseCurrent()
        

    let info =
        useContext (fun () ->
            DeviceInfo.read gl
        )

    let directState =
        useContext <| fun () ->
            match gl.TryGetExtension<ArbDirectStateAccess>() with
            | true, e -> Some e
            | _ -> None
            
    let bufferStorage =
        useContext <| fun () ->
            match gl.TryGetExtension<ArbBufferStorage>() with
            | true, e -> Some e
            | _ -> None
    let copyBuffer =
        useContext <| fun () ->
            match gl.TryGetExtension<ArbCopyBuffer>() with
            | true, e -> Some e
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
    member x.CopyBuffer = copyBuffer

    member x.Info = info

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
 
    override x.Start(cmd : CommandStream) =
        x.Start (unbox<ManagedOpenGLCommandStream> cmd).Run
        
    override x.StartAsTask(cmd : CommandStream) =
        x.StartAsTask (unbox<ManagedOpenGLCommandStream> cmd).Run :> Task
        
    override x.Run(cmd : CommandStream) =
        x.Run (unbox<ManagedOpenGLCommandStream> cmd).Run

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
        new ManagedOpenGLCommandStream() :> CommandStream

and ManagedOpenGLCommandStream() =
    inherit CommandStream()

    let actions = System.Collections.Generic.List<GL -> unit>()
    let cleanup = System.Collections.Generic.List<unit -> unit>()


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
            actions.Add <| fun gl ->
                match gl.TryGetExtension<ArbDirectStateAccess>() with
                | true, ext ->
                    let pdst = ext.MapNamedBufferRange(dh, int dstOffset, uint32 size, uint32 (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit)) |> VoidPtr.toNativeInt
                    Marshal.Copy(src, pdst, size)
                    ext.UnmapNamedBuffer(dh) |> ignore
                | _ ->
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
        for c in cleanup do c()
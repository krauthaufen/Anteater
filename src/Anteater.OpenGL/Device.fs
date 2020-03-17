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

type Device(cfg : DeviceConfig) =
    do if cfg.nVidia then DynamicLinker.tryLoadLibrary ("nvapi64" + libraryExtension) |> ignore


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

    let info =
        contexts.[0].MakeCurrent()
        let res = DeviceInfo.read gl
        contexts.[0].ReleaseCurrent()
        res

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

    member x.Info = info

    member x.Dispose() =
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
 
    member x.Start(cmd : CommandStream) =
        x.Start cmd.Run
        
    member x.StartAsTask(cmd : CommandStream) =
        x.StartAsTask cmd.Run
        
    member x.Run(cmd : CommandStream) =
        x.Run cmd.Run

    interface IDisposable with
        member x.Dispose() = x.Dispose()


and CommandStream() =
    let actions = System.Collections.Generic.List<GL -> unit>()
    let cleanup = System.Collections.Generic.List<unit -> unit>()


    member x.Copy(src : Anteater.Buffer, srcOffset : int64, dst : Anteater.Buffer, dstOffset : int64, size : int64) =
        let sh = unbox<uint32> src.Handle
        let dh = unbox<uint32> dst.Handle
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
                    let ps = gl.MapBuffer(BufferTargetARB.ArrayBuffer, BufferAccessARB.ReadOnly)
                    let pd = gl.MapBuffer(BufferTargetARB.ElementArrayBuffer, BufferAccessARB.WriteOnly)

                    let psrc = NativePtr.toNativeInt (NativePtr.ofVoidPtr<byte> ps)
                    let pdst = NativePtr.toNativeInt (NativePtr.ofVoidPtr<byte> pd)
                    Marshal.Copy(psrc + nativeint srcOffset, pdst + nativeint dstOffset, size)
                    gl.UnmapBuffer BufferTargetARB.ArrayBuffer |> ignore
                    gl.UnmapBuffer BufferTargetARB.ElementArrayBuffer |> ignore

    member x.Copy(src : nativeint, dst : Anteater.Buffer, dstOffset : int64, size : int64) =
        let dh = unbox<uint32> dst.Handle
        actions.Add <| fun gl ->
            match gl.TryGetExtension<ArbDirectStateAccess>() with
            | true, ext ->
                let pd = ext.MapNamedBufferRange(dh, int dstOffset, uint32 size, uint32 MapBufferAccessMask.MapWriteBit)
                let pdst = NativePtr.toNativeInt (NativePtr.ofVoidPtr<byte> pd)
                Marshal.Copy(src, pdst + nativeint dstOffset, size)
                ext.UnmapNamedBuffer(dh) |> ignore
            | _ ->
                gl.BindBuffer(BufferTargetARB.ArrayBuffer, dh)
                let pd = gl.MapBufferRange(BufferTargetARB.ArrayBuffer, nativeint dstOffset, unativeint size, uint32 (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit))
                let pdst = NativePtr.toNativeInt (NativePtr.ofVoidPtr<byte> pd)
                Marshal.Copy(src, pdst + nativeint dstOffset, size)
                gl.UnmapBuffer BufferTargetARB.ArrayBuffer |> ignore
                
    member x.Copy(src : Anteater.Buffer, srcOffset : int64, dst : nativeint, size : int64) =
        let sh = unbox<uint32> src.Handle
        actions.Add <| fun gl ->
            match gl.TryGetExtension<ArbDirectStateAccess>() with
            | true, ext ->
                let ps = ext.MapNamedBufferRange(sh, int srcOffset, uint32 size, uint32 MapBufferAccessMask.MapReadBit)
                let psrc = NativePtr.toNativeInt (NativePtr.ofVoidPtr<byte> ps)
                Marshal.Copy(psrc, dst, size)
                ext.UnmapNamedBuffer(sh) |> ignore
            | _ ->
                gl.BindBuffer(BufferTargetARB.ArrayBuffer, sh)
                let ps = gl.MapBufferRange(BufferTargetARB.ArrayBuffer, nativeint srcOffset, unativeint size, uint32 MapBufferAccessMask.MapReadBit)
                let psrc = NativePtr.toNativeInt (NativePtr.ofVoidPtr<byte> ps)
                Marshal.Copy(psrc, dst, size)
                gl.UnmapBuffer BufferTargetARB.ArrayBuffer |> ignore

    member x.Copy<'a when 'a : unmanaged>(src : 'a[], srcIndex : int, dst : Anteater.Buffer, dstOffset : int64, count : int) =
        let gc = GCHandle.Alloc(src, GCHandleType.Pinned)
        let sa = int64 sizeof<'a>
        let srcOffset = int64 srcIndex * sa
        let size = int64 count * sa
        x.Copy(gc.AddrOfPinnedObject() + nativeint srcOffset, dst, dstOffset, size)
        actions.Add (ignore >> gc.Free)

    member x.Copy<'a when 'a : unmanaged>(src : Anteater.Buffer, srcOffset : int64, dst : 'a[], dstIndex : int, count : int) =
        let gc = GCHandle.Alloc(dst, GCHandleType.Pinned)
        let sa = int64 sizeof<'a>
        let dstOffset = int64 dstIndex * sa
        let size = int64 count * sa
        x.Copy(src, srcOffset, gc.AddrOfPinnedObject() + nativeint dstOffset, size)
        actions.Add (ignore >> gc.Free)

    member x.Copy<'a when 'a : unmanaged>(src : 'a[], dst : Anteater.Buffer) =
        x.Copy(src, 0, dst, 0L, src.Length)
        
    member x.Copy<'a when 'a : unmanaged>(src : Anteater.Buffer, dst : 'a[]) =
        x.Copy(src, 0L, dst, 0, dst.Length)

    member internal x.Run(gl : GL) =
        for a in actions do a gl
        for c in cleanup do c()
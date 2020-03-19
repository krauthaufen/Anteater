namespace Anteater.OpenGL


open System
open System.Runtime.InteropServices
open Aardvark.Base
open Silk.NET.OpenGL
open Silk.NET.OpenGL.Extensions.ARB
open Anteater

[<CommandStreamScore(1)>]
type internal ManagedOpenGLCommandStream(device : OpenGLDevice) =
    inherit OpenGLCommandStream()

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

    override x.Run(ctx : ContextHandle, gl : GL) =
        for a in actions do a gl

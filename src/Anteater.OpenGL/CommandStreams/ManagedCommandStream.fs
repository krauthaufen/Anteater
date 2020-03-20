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


    override x.Copy(src : ImageSubresourceRegion, dst : ImageSubresourceRegion) : unit =
        if src.Size <> dst.Size then
            failwithf "[GL] mismatching size for image copy: %A vs %A" src.Size dst.Size
            
        let srcImage = src.Resource.Image
        let srcHandle = src.Resource.Image.Handle |> unbox<uint32>
        let srcLevel = src.Resource.Level
        let srcSlice = src.Resource.BaseSlice
        let srcOffset = src.Offset
        
        let dstImage = dst.Resource.Image
        let dstHandle = dst.Resource.Image.Handle |> unbox<uint32>
        let dstLevel = dst.Resource.Level
        let dstSlice = dst.Resource.BaseSlice
        let dstOffset = dst.Offset

        let slices = min src.Resource.Slices dst.Resource.Slices


        let mutable size = src.Size
        
        let mutable srcTarget = CopyImageSubDataTarget.Renderbuffer
        let mutable srcOffset = srcOffset
        match srcImage.Dimension with
        | ImageDimension.Image1d _ ->
            if srcImage.IsArray then
                srcTarget <- CopyImageSubDataTarget.Texture1DArray
                srcOffset <- V3i(srcOffset.X, srcSlice, 0)
                size <- V3i(size.X, slices, 1)
            else
                srcTarget <- CopyImageSubDataTarget.Texture1D
                srcOffset <- V3i(srcOffset.X, 0, 0)
                size <- V3i(size.X, 1, 1)
                
        | ImageDimension.Image2d _ ->
            if srcImage.IsArray then
                if srcImage.Samples > 1 then srcTarget <- CopyImageSubDataTarget.Texture2DMultisampleArray
                else srcTarget <- CopyImageSubDataTarget.Texture2DArray
                srcOffset <- V3i(srcOffset.X, srcOffset.Y, srcSlice)
                size <- V3i(size.X, size.Y, slices)
            else
                if srcImage.Samples > 1 then srcTarget <- CopyImageSubDataTarget.Texture2DMultisample
                else srcTarget <- CopyImageSubDataTarget.Texture2D
                srcOffset <- V3i(srcOffset.X, srcOffset.Y, 0)
                size <- V3i(size.X, size.Y, 1)
                
        | ImageDimension.Image3d _ ->
            srcTarget <- CopyImageSubDataTarget.Texture3D

        | ImageDimension.ImageCube _ ->
            if srcImage.IsArray then
                srcTarget <- CopyImageSubDataTarget.TextureCubeMapArray
                srcOffset <- V3i(srcOffset.X, srcOffset.Y, srcSlice * 6)
                size <- V3i(size.X, size.Y, slices * 6)
            else    
                srcTarget <- CopyImageSubDataTarget.TextureCubeMap
                srcOffset <- V3i(srcOffset.X, srcOffset.Y, 0)
                size <- V3i(size.X, size.Y, 6)
            
        let srcOffset = srcOffset
        let srcTarget = srcTarget
        let srcSlice = ()

        let mutable dstTarget = CopyImageSubDataTarget.Renderbuffer
        let mutable dstOffset = dstOffset
        match dstImage.Dimension with
        | ImageDimension.Image1d _ ->
            if srcImage.IsArray then
                dstTarget <- CopyImageSubDataTarget.Texture1DArray
                dstOffset <- V3i(dstOffset.X, dstSlice, 0)
            else
                dstTarget <- CopyImageSubDataTarget.Texture1D
                dstOffset <- V3i(dstOffset.X, 0, 0)
                
        | ImageDimension.Image2d _ ->
            if srcImage.IsArray then
                if srcImage.Samples > 1 then dstTarget <- CopyImageSubDataTarget.Texture2DMultisampleArray
                else dstTarget <- CopyImageSubDataTarget.Texture2DArray
                dstOffset <- V3i(dstOffset.X, dstOffset.Y, dstSlice)
            else
                if srcImage.Samples > 1 then dstTarget <- CopyImageSubDataTarget.Texture2DMultisample
                else dstTarget <- CopyImageSubDataTarget.Texture2D
                dstOffset <- V3i(dstOffset.X, dstOffset.Y, 0)
                
        | ImageDimension.Image3d _ ->
            dstTarget <- CopyImageSubDataTarget.Texture3D

        | ImageDimension.ImageCube _ ->
            if srcImage.IsArray then
                dstTarget <- CopyImageSubDataTarget.TextureCubeMapArray
                dstOffset <- V3i(dstOffset.X, dstOffset.Y, dstSlice * 6)
            else    
                dstTarget <- CopyImageSubDataTarget.TextureCubeMap
                dstOffset <- V3i(dstOffset.X, dstOffset.Y, 0)
            
        let dstOffset = dstOffset
        let dstTarget = dstTarget


        actions.Add <| fun gl ->
            match device.CopyImage with
            | Some ext ->
                ext.CopyImageSubData(
                    srcHandle, srcTarget, srcLevel, srcOffset.X, srcOffset.Y, srcOffset.Z,
                    dstHandle, dstTarget, dstLevel, dstOffset.X, dstOffset.Y, dstOffset.Z,
                    uint32 size.X, uint32 size.Y, uint32 size.Z
                )
            | _ -> 
                failwith "GL] not implemented"

    override x.Run(ctx : ContextHandle, gl : GL) =
        for a in actions do a gl

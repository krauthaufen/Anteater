namespace Anteater.OpenGL


open System
open Microsoft.FSharp.NativeInterop
open System.Runtime.InteropServices
open Aardvark.Base
open Silk.NET.OpenGL
open Silk.NET.OpenGL.Extensions.ARB
open Anteater

#nowarn "9"

[<CommandStreamScore(100)>]
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

    override x.Copy<'T when 'T : unmanaged>(src : nativeptr<'T>, fmt : Col.Format, dst : ImageSubresourceRegion) : unit =
        let dstResource = dst.Resource
        let dstImage = dstResource.Image
        let dh = unbox<uint32> dstImage.Handle
        let dstLevel = dstResource.Level

        let channels = fmt.Channels
        let fmt = fmt.PixelFormat
        let typ = typeof<'T>.PixelType
        actions.Add <| fun gl ->
            match device.DirectState with
            | Some ext ->
                match dstImage.Dimension with
                | ImageDimension.Image1d _ when not dstImage.IsArray ->
                    ext.TextureSubImage1D(dh, dstLevel, dst.Offset.X, uint32 dst.Size.X, fmt, typ, VoidPtr.ofNativePtr src)
                | ImageDimension.Image1d _ ->
                    ext.TextureSubImage2D(dh, dstLevel, dst.Offset.X, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dstResource.Slices, fmt, typ, VoidPtr.ofNativePtr src)
                    
                | ImageDimension.Image2d _ when not dstImage.IsArray ->
                    ext.TextureSubImage2D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, uint32 dst.Size.X, uint32 dst.Size.Y, fmt, typ, VoidPtr.ofNativePtr src)
                | ImageDimension.Image2d _ ->
                    ext.TextureSubImage3D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dstResource.Slices, fmt, typ, VoidPtr.ofNativePtr src)

                | ImageDimension.Image3d _ ->
                    ext.TextureSubImage3D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, dst.Offset.Z, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dst.Size.Z, fmt, typ, VoidPtr.ofNativePtr src)
                    
                | ImageDimension.ImageCube _ when not dstImage.IsArray ->
                    ext.TextureSubImage3D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, 0, uint32 dst.Size.X, uint32 dst.Size.Y, 6u, fmt, typ, VoidPtr.ofNativePtr src)

                | ImageDimension.ImageCube _ ->
                    ext.TextureSubImage3D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dstResource.Slices, fmt, typ, VoidPtr.ofNativePtr src)
            | None ->
                match dstImage.Dimension with
                | ImageDimension.Image1d _ when not dstImage.IsArray ->
                    gl.BindTexture(TextureTarget.Texture1D, dh)
                    gl.TexSubImage1D(TextureTarget.Texture1D, dstLevel, dst.Offset.X, uint32 dst.Size.X, fmt, typ, VoidPtr.ofNativePtr src)
                    gl.BindTexture(TextureTarget.Texture1D, 0u)

                | ImageDimension.Image1d _ ->
                    gl.BindTexture(TextureTarget.Texture1DArray, dh)
                    gl.TexSubImage2D(TextureTarget.Texture1DArray, dstLevel, dst.Offset.X, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dstResource.Slices, fmt, typ, VoidPtr.ofNativePtr src)
                    gl.BindTexture(TextureTarget.Texture1D, 0u)
                    
                | ImageDimension.Image2d _ when not dstImage.IsArray ->
                    gl.BindTexture(TextureTarget.Texture2D, dh)
                    gl.TexSubImage2D(TextureTarget.Texture2D, dstLevel, dst.Offset.X, dst.Offset.Y, uint32 dst.Size.X, uint32 dst.Size.Y, fmt, typ, VoidPtr.ofNativePtr src)
                    gl.BindTexture(TextureTarget.Texture2D, 0u)

                | ImageDimension.Image2d _ ->
                    gl.BindTexture(TextureTarget.Texture2DArray, dh)
                    gl.TexSubImage3D(TextureTarget.Texture2DArray, dstLevel, dst.Offset.X, dst.Offset.Y, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dstResource.Slices, fmt, typ, VoidPtr.ofNativePtr src)
                    gl.BindTexture(TextureTarget.Texture2DArray, 0u)

                | ImageDimension.Image3d _ ->
                    gl.BindTexture(TextureTarget.Texture3D, dh)
                    gl.TexSubImage3D(TextureTarget.Texture3D, dstLevel, dst.Offset.X, dst.Offset.Y, dst.Offset.Z, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dst.Size.Z, fmt, typ, VoidPtr.ofNativePtr src)
                    gl.BindTexture(TextureTarget.Texture3D, 0u)

                | ImageDimension.ImageCube _ when not dstImage.IsArray ->
                    gl.BindTexture(TextureTarget.TextureCubeMap, dh)
                    let mutable ptr = NativePtr.toNativeInt src
                    let sliceSize = nativeint dst.Size.X * nativeint dst.Size.Y * nativeint channels
                    for face in int TextureTarget.TextureCubeMapPositiveX .. int TextureTarget.TextureCubeMapNegativeZ do
                        gl.TexSubImage2D(unbox<TextureTarget> face, dstLevel, dst.Offset.X, dst.Offset.Y, uint32 dst.Size.X, uint32 dst.Size.Y, fmt, typ, VoidPtr.ofNativeInt ptr)
                        ptr <- ptr + sliceSize
                    gl.BindTexture(TextureTarget.TextureCubeMap, 0u)

                | ImageDimension.ImageCube _ ->
                    gl.BindTexture(TextureTarget.TextureCubeMapArray, dh)
                    gl.TexSubImage3D(TextureTarget.TextureCubeMapArray, dstLevel, dst.Offset.X, dst.Offset.Y, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dstResource.Slices, fmt, typ, VoidPtr.ofNativePtr src)
                    gl.BindTexture(TextureTarget.TextureCubeMapArray, 0u)
                
        ()

    override x.Run(ctx : ContextHandle, gl : GL) =
        for a in actions do a gl

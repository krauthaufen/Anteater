namespace Anteater.OpenGL


open System
open Microsoft.FSharp.NativeInterop
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Aardvark.Base
open Silk.NET.OpenGL
open Silk.NET.OpenGL.Extensions.ARB
open Anteater

#nowarn "9"

[<AbstractClass; Sealed; Extension>]
type internal TexturePBOExtensions private() =
    [<Extension>]
    static member CreatePixelBuffer<'T when 'T : unmanaged>(device : OpenGLDevice, channels : int, dst : ImageSubresourceRegion, write : bool) =

        let dstResource = dst.Resource
        let dstImage = dstResource.Image
        let slices = dstResource.Slices

        let copySize = dst.Size
        let sizeInBytes = nativeint copySize.X * nativeint copySize.Y * nativeint copySize.Z * nativeint channels * nativeint sizeof<'T>

        let info =
            match dstImage.Dimension with
            | ImageDimension.Image1d _ ->
                Tensor4Info(0L, V4l(copySize.X, channels, slices, 1), V4l(int64 channels, 1L, int64 channels * int64 copySize.X, 1L))
            | ImageDimension.Image2d _ ->
                Tensor4Info(0L, V4l(copySize.X, copySize.Y, channels, slices), V4l(int64 channels, int64 channels * int64 copySize.X, 1L, int64 channels * int64 copySize.X * int64 copySize.Y))
            | ImageDimension.ImageCube _ ->
                Tensor4Info(0L, V4l(copySize.X, copySize.Y, channels, 6 * slices), V4l(int64 channels, int64 channels * int64 copySize.X, 1L, int64 channels * int64 copySize.X * int64 copySize.Y))
            | ImageDimension.Image3d _ ->
                Tensor4Info(0L, V4l(copySize.X, copySize.Y, copySize.Z, channels), V4l(int64 channels, int64 channels * int64 copySize.X, int64 channels * int64 copySize.X * int64 copySize.Y, 1L))
                    

        let storeMask =
            if write then BufferStorageMask.MapWriteBit ||| BufferStorageMask.ClientStorageBit
            else BufferStorageMask.MapReadBit ||| BufferStorageMask.ClientStorageBit

        let mapMask =
            if write then MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateBufferBit
            else MapBufferAccessMask.MapReadBit


        match device.DirectState with
        | Some ext ->

            let temp = ext.CreateBuffer()
            ext.NamedBufferStorage(temp, unativeint sizeInBytes, VoidPtr.zero, uint32 storeMask)
            let map() =
                let tempPtr = ext.MapNamedBufferRange(temp, 0n, unativeint sizeInBytes, uint32 mapMask) |> VoidPtr.toNativePtr<'T>
                NativeTensor4<'T>(tempPtr, info)

            let unmap() =
                ext.UnmapNamedBuffer(temp)

            temp, map, unmap
        | None ->
            let gl = device.GL
            let temp = gl.GenBuffer()
            gl.BindBuffer(BufferTargetARB.ArrayBuffer, temp)
            match device.BufferStorage with
            | Some ext ->
                ext.BufferStorage(BufferStorageTarget.ArrayBuffer, unativeint sizeInBytes, VoidPtr.zero, uint32 storeMask)
                gl.BindBuffer(BufferTargetARB.ArrayBuffer, 0u)
                let map() =
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, temp)
                    let tempPtr = gl.MapBufferRange(BufferTargetARB.ArrayBuffer, 0n, unativeint sizeInBytes, uint32 mapMask) |> VoidPtr.toNativePtr<'T>
                    NativeTensor4<'T>(tempPtr, info)

                let unmap() =
                    let res = gl.UnmapBuffer(BufferTargetARB.ArrayBuffer)
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, 0u)
                    res
                    
                temp, map, unmap
            | None ->
                failwith ""



[<CommandStreamScore(100)>]
type internal ManagedOpenGLCommandStream(device : OpenGLDevice) =
    inherit OpenGLCommandStream()

    let actions = System.Collections.Generic.List<GL -> unit>()

    member private x.CheckCopy<'T> (host : Tensor4Info, server : ImageSubresourceRegion) =

        let srcFormat = server.Resource.Image.Format
        if not (HashSet.contains (typeof<'T>) (ImageFormat.channelTypes srcFormat)) then
            failwithf "[GL] cannot copy between image with format %A and Tensor<%s>" srcFormat typeof<'T>.Name
        
        let srcChannels = ImageFormat.channels srcFormat
        let dstChannels =
            match server.Resource.Image.Dimension with
            | ImageDimension.Image1d _ -> 
                let dstSize = V2i(host.SX, host.SZ)
                let srcSize = V2i(server.Size.X, server.Resource.Slices)
                if dstSize <> srcSize then failwithf "[GL] mismatching sizes %A vs %A" dstSize srcSize
                int host.SY
            | ImageDimension.Image2d _ -> 
                let dstSize = V3i(host.SX, host.SY, host.SW)
                let srcSize = V3i(server.Size.X, server.Size.Y, server.Resource.Slices)
                if dstSize <> srcSize then failwithf "[GL] mismatching sizes %A vs %A" dstSize srcSize
                int host.SZ
            | ImageDimension.ImageCube _ -> 
                let dstSize = V3i(host.SX, host.SY, max 1L (host.SW / 6L))
                let srcSize = V3i(server.Size.X, server.Size.Y, server.Resource.Slices)
                if dstSize <> srcSize then failwithf "[GL] mismatching sizes %A vs %A" dstSize srcSize
                int host.SZ
            | ImageDimension.Image3d _ -> 
                let dstSize = V3i host.Size.XYZ
                let srcSize = server.Size
                if dstSize <> srcSize then failwithf "[GL] mismatching sizes %A vs %A" dstSize srcSize
                int host.SW
        if dstChannels <> srcChannels then
            failwithf "[GL] cannot copy between image with %d and Tensor with %d channels" srcChannels dstChannels



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

    member x.Copy(acquire : unit -> nativeint * (unit -> unit), dst : BufferRange) =
        let size = dst.Size
        if size > 0L then
            let dstOffset = dst.Offset
            let dh = unbox<uint32> dst.Buffer.Handle
            match device.DirectState with
            | Some ext ->
                actions.Add <| fun gl ->
                    let pdst = ext.MapNamedBufferRange(dh, int dstOffset, uint32 size, uint32 (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit)) |> VoidPtr.toNativeInt
                    let src, release = acquire()
                    Marshal.Copy(src, pdst, size)
                    release()
                    ext.UnmapNamedBuffer(dh) |> ignore
            | None -> 
                actions.Add <| fun gl ->
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, dh)
                    let pdst = gl.MapBufferRange(BufferTargetARB.ArrayBuffer, nativeint dstOffset, unativeint size, uint32 (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit)) |> VoidPtr.toNativeInt
                    let src, release = acquire()
                    Marshal.Copy(src, pdst, size)
                    release()
                    gl.UnmapBuffer BufferTargetARB.ArrayBuffer |> ignore
                    
    member x.Copy(src : BufferRange, acquire : unit -> nativeint * (unit -> unit)) =
        let size = src.Size
        if size > 0L then
            let sh = unbox<uint32> src.Buffer.Handle
            actions.Add <| fun gl ->
                match gl.TryGetExtension<ArbDirectStateAccess>() with
                | true, ext ->
                    let psrc = ext.MapNamedBufferRange(sh, int src.Offset, uint32 size, uint32 MapBufferAccessMask.MapReadBit) |> VoidPtr.toNativeInt
                    let dst, release = acquire()
                    Marshal.Copy(psrc, dst, size)
                    release()
                    ext.UnmapNamedBuffer(sh) |> ignore
                | _ ->
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, sh)
                    let psrc = gl.MapBufferRange(BufferTargetARB.ArrayBuffer, nativeint src.Offset, unativeint size, uint32 MapBufferAccessMask.MapReadBit) |> VoidPtr.toNativeInt
                    let dst, release = acquire()
                    Marshal.Copy(psrc, dst, size)
                    release()
                    gl.UnmapBuffer BufferTargetARB.ArrayBuffer |> ignore

    override x.Copy(src : nativeint, dst : BufferRange) =
        x.Copy(
            (fun () -> src, id),
            dst
        )

    override x.Copy(src : BufferRange, dst : nativeint) =
        x.Copy(
            src,
            (fun () -> dst, id)
        )

    override x.Copy<'T when 'T : unmanaged>(src : Memory<'T>, dst : Anteater.BufferRange) =
        let pin() =
            let h = src.Pin()
            VoidPtr.toNativeInt h.Pointer, h.Dispose
        x.Copy(pin, dst)
      
    override x.Copy<'T when 'T : unmanaged>(src : Anteater.BufferRange, dst : Memory<'T>) =
        let pin() =
            let h = dst.Pin()
            VoidPtr.toNativeInt h.Pointer, h.Dispose
        x.Copy(src, pin)


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

    member x.Copy<'T when 'T : unmanaged>(acquire : unit -> NativeTensor4<'T> * (unit -> unit), dst : ImageSubresourceRegion) : unit =
        let dstResource = dst.Resource
        let dstImage = dstResource.Image
        let dh = unbox<uint32> dstImage.Handle
        let dstLevel = dstResource.Level

        let channels = ImageFormat.channels dstImage.Format
        let fmt = dstImage.Format.PixelFormat
        let typ = typeof<'T>.PixelType

        actions.Add <| fun gl ->
            let temp, map, unmap = device.CreatePixelBuffer(channels, dst, true)
            let src, release = acquire()
            let dstTensor = map()
            NativeTensor4.copy src dstTensor
            unmap() |> ignore
            release()

            match device.DirectState with
            | Some ext ->
                gl.BindBuffer(BufferTargetARB.PixelUnpackBuffer, temp)

                match dstImage.Dimension with
                | ImageDimension.Image1d _ when not dstImage.IsArray ->
                    ext.TextureSubImage1D(dh, dstLevel, dst.Offset.X, uint32 dst.Size.X, fmt, typ, VoidPtr.zero)
                | ImageDimension.Image1d _ ->
                    ext.TextureSubImage2D(dh, dstLevel, dst.Offset.X, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dstResource.Slices, fmt, typ, VoidPtr.zero)
                    
                | ImageDimension.Image2d _ when not dstImage.IsArray ->
                    ext.TextureSubImage2D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, uint32 dst.Size.X, uint32 dst.Size.Y, fmt, typ, VoidPtr.zero)
                | ImageDimension.Image2d _ ->
                    ext.TextureSubImage3D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dstResource.Slices, fmt, typ, VoidPtr.zero)

                | ImageDimension.Image3d _ ->
                    ext.TextureSubImage3D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, dst.Offset.Z, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dst.Size.Z, fmt, typ, VoidPtr.zero)
                    
                | ImageDimension.ImageCube _ when not dstImage.IsArray ->
                    ext.TextureSubImage3D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, 0, uint32 dst.Size.X, uint32 dst.Size.Y, 6u, fmt, typ, VoidPtr.zero)

                | ImageDimension.ImageCube _ ->
                    ext.TextureSubImage3D(dh, dstLevel, dst.Offset.X, dst.Offset.Y, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dstResource.Slices, fmt, typ, VoidPtr.zero)
                
                gl.BindBuffer(BufferTargetARB.PixelUnpackBuffer, 0u)
                gl.DeleteBuffer temp



            | None ->
                gl.BindBuffer(BufferTargetARB.PixelUnpackBuffer, temp)

                match dstImage.Dimension with
                | ImageDimension.Image1d _ when not dstImage.IsArray ->
                    gl.BindTexture(TextureTarget.Texture1D, dh)
                    gl.TexSubImage1D(TextureTarget.Texture1D, dstLevel, dst.Offset.X, uint32 dst.Size.X, fmt, typ, VoidPtr.zero)
                    gl.BindTexture(TextureTarget.Texture1D, 0u)

                | ImageDimension.Image1d _ ->
                    gl.BindTexture(TextureTarget.Texture1DArray, dh)
                    gl.TexSubImage2D(TextureTarget.Texture1DArray, dstLevel, dst.Offset.X, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dstResource.Slices, fmt, typ, VoidPtr.zero)
                    gl.BindTexture(TextureTarget.Texture1D, 0u)
                    
                | ImageDimension.Image2d _ when not dstImage.IsArray ->
                    gl.BindTexture(TextureTarget.Texture2D, dh)
                    gl.TexSubImage2D(TextureTarget.Texture2D, dstLevel, dst.Offset.X, dst.Offset.Y, uint32 dst.Size.X, uint32 dst.Size.Y, fmt, typ, VoidPtr.zero)
                    gl.BindTexture(TextureTarget.Texture2D, 0u)

                | ImageDimension.Image2d _ ->
                    gl.BindTexture(TextureTarget.Texture2DArray, dh)
                    gl.TexSubImage3D(TextureTarget.Texture2DArray, dstLevel, dst.Offset.X, dst.Offset.Y, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dstResource.Slices, fmt, typ, VoidPtr.zero)
                    gl.BindTexture(TextureTarget.Texture2DArray, 0u)

                | ImageDimension.Image3d _ ->
                    gl.BindTexture(TextureTarget.Texture3D, dh)
                    gl.TexSubImage3D(TextureTarget.Texture3D, dstLevel, dst.Offset.X, dst.Offset.Y, dst.Offset.Z, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dst.Size.Z, fmt, typ, VoidPtr.zero)
                    gl.BindTexture(TextureTarget.Texture3D, 0u)

                | ImageDimension.ImageCube _ when not dstImage.IsArray ->
                    gl.BindTexture(TextureTarget.TextureCubeMap, dh)
                    let mutable z = 0
                    for face in int TextureTarget.TextureCubeMapPositiveX .. int TextureTarget.TextureCubeMapNegativeZ do
                        let offset = Vec.dot (V4l(0,0,0,z)) dstTensor.Delta
                        gl.TexSubImage2D(unbox<TextureTarget> face, dstLevel, dst.Offset.X, dst.Offset.Y, uint32 dst.Size.X, uint32 dst.Size.Y, fmt, typ, VoidPtr.ofNativeInt (nativeint offset))
                        z <- z + 1
                    gl.BindTexture(TextureTarget.TextureCubeMap, 0u)

                | ImageDimension.ImageCube _ ->
                    gl.BindTexture(TextureTarget.TextureCubeMapArray, dh)
                    gl.TexSubImage3D(TextureTarget.TextureCubeMapArray, dstLevel, dst.Offset.X, dst.Offset.Y, dstResource.BaseSlice, uint32 dst.Size.X, uint32 dst.Size.Y, uint32 dstResource.Slices, fmt, typ, VoidPtr.zero)
                    gl.BindTexture(TextureTarget.TextureCubeMapArray, 0u)
                
                gl.BindBuffer(BufferTargetARB.PixelUnpackBuffer, 0u)
                gl.DeleteBuffer temp

    member x.Copy<'T when 'T : unmanaged>(src : ImageSubresourceRegion, acquire : unit -> NativeTensor4<'T> * (unit -> unit)) =
        
        let srcResource = src.Resource
        let srcImage = srcResource.Image
        let sh = unbox<uint32> srcImage.Handle
        let srcLevel = srcResource.Level

        let channels = ImageFormat.channels srcImage.Format
        let fmt = srcImage.Format.PixelFormat
        let typ = typeof<'T>.PixelType
        
        let sliceSize = int64 src.Size.X * int64 src.Size.Y * int64 src.Size.Z * int64 channels * int64 sizeof<'T>
        let bufferSize = 
            match srcImage.Dimension with
            | ImageDimension.ImageCube _ -> 6L * sliceSize * int64 srcResource.Slices
            | _ -> sliceSize * int64 srcResource.Slices

        if src.Offset = V3i.Zero && src.Size = srcImage.Size && srcResource.Slices = srcImage.Slices then
            actions.Add <| fun gl ->
                let temp, map, unmap = device.CreatePixelBuffer(channels, src, false)
                gl.BindBuffer(BufferTargetARB.PixelPackBuffer, temp)
                match device.DirectState with
                | Some ext ->
                    ext.GetTextureImage(sh, srcLevel, fmt, typ, uint32 bufferSize, VoidPtr.zero)
                | None ->
                    let mutable offset = 0n
                    let info = srcImage.Target
                    gl.BindTexture(info.target, sh)
                    for target in info.targets do
                        gl.GetTexImage(target, srcLevel, fmt, typ, VoidPtr.ofNativeInt offset)
                        offset <- offset + nativeint sliceSize
                    gl.BindTexture(info.target, 0u)
                gl.BindBuffer(BufferTargetARB.PixelPackBuffer, 0u)

                let dst, release = acquire()
                let tempTensor = map()
                NativeTensor4.copy tempTensor dst
                unmap() |> ignore
                release()
                gl.DeleteBuffer temp

        else
            match srcImage.Dimension with
            | ImageDimension.Image2d _ ->
                actions.Add <| fun gl ->
                    let fbo = gl.GenFramebuffer()
                    gl.BindFramebuffer(FramebufferTarget.ReadFramebuffer, fbo)

                    let buffer, map, unmap = device.CreatePixelBuffer<'T>(channels, src, false)

                    gl.BindBuffer(BufferTargetARB.PixelPackBuffer, buffer)
                    let mutable offset = 0n
                    for slice in srcResource.BaseSlice .. srcResource.BaseSlice + srcResource.Slices - 1 do
                        let att = 
                            if ImageFormat.hasDepth srcImage.Format then
                                if ImageFormat.hasStencil srcImage.Format then unbox<FramebufferAttachment>(int GLEnum.DepthStencilAttachment)
                                else unbox<FramebufferAttachment>(int GLEnum.DepthAttachment)
                            else
                                gl.ReadBuffer(ReadBufferMode.ColorAttachment0)
                                FramebufferAttachment.ColorAttachment0

                        if srcImage.IsArray then
                            gl.FramebufferTextureLayer(FramebufferTarget.ReadFramebuffer, att, sh, srcResource.Level, slice)
                        else
                            gl.FramebufferTexture2D(FramebufferTarget.ReadFramebuffer, att, TextureTarget.Texture2D, sh, srcResource.Level)

                        let status = gl.CheckFramebufferStatus(FramebufferTarget.ReadFramebuffer)
                        if status <> GLEnum.FramebufferComplete then failwithf "[GL] framebuffer error: %A" status

                        gl.ReadPixels(src.Offset.X, src.Offset.Y, uint32 src.Size.X, uint32 src.Size.Y, fmt, typ, VoidPtr.ofNativeInt offset)
                        offset <- offset + nativeint sliceSize
                    gl.BindBuffer(BufferTargetARB.PixelPackBuffer, 0u)



                    gl.BindFramebuffer(FramebufferTarget.ReadFramebuffer, 0u)
                    gl.DeleteFramebuffer fbo

                    let src = map()
                    let dst, release = acquire()
                    NativeTensor4.copy src dst
                    unmap() |> ignore
                    release()


                    gl.DeleteBuffer buffer

            | _ -> 
                failwith "sub-region download not implemented atm."

    
    override x.Copy<'T when 'T : unmanaged>(src : NativeTensor4<'T>, dst : ImageSubresourceRegion) =  
        x.CheckCopy<'T>(src.Info, dst)
        x.Copy(
            (fun () -> src, id), 
            dst
        )

    override x.Copy<'T when 'T : unmanaged>(src : Tensor4<'T>, dst : ImageSubresourceRegion) =
        x.CheckCopy<'T>(src.Info, dst)
        let pin() =
            let gc = GCHandle.Alloc(src.Data, GCHandleType.Pinned)
            let tensor =
                NativeTensor4<'T>(
                    NativePtr.ofNativeInt (gc.AddrOfPinnedObject()),
                    src.Info
                )
            tensor, gc.Free
        x.Copy(pin, dst)
        
    override x.Copy<'T when 'T : unmanaged>(src : ImageSubresourceRegion, dst : NativeTensor4<'T>) =
        x.CheckCopy<'T>(dst.Info, src)
        x.Copy(src,
            (fun () -> dst, id)
        )
    
    override x.Copy<'T when 'T : unmanaged>(src : ImageSubresourceRegion, dst : Tensor4<'T>) =
        x.CheckCopy<'T>(dst.Info, src)
        let pin() =
            let gc = GCHandle.Alloc(dst.Data, GCHandleType.Pinned)
            let tensor =
                NativeTensor4<'T>(
                    NativePtr.ofNativeInt (gc.AddrOfPinnedObject()),
                    dst.Info
                )
            tensor, gc.Free
        x.Copy(src, pin)

    override x.Run(ctx : ContextHandle, gl : GL) =
        for a in actions do a gl

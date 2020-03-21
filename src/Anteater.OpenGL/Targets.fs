namespace Anteater.OpenGL


open System
open System.Runtime.InteropServices
open Aardvark.Base
open Silk.NET.OpenGL
open Silk.NET.OpenGL.Extensions.ARB
open Anteater

[<Struct>]
type TargetDescription =
    {
        target          : TextureTarget
        targets         : list<TextureTarget>
        storeDimension  : int
        imageDimension  : int
        array           : bool
        multisampled    : bool
    }

[<AutoOpen>]
module ImageTargetExtensions =
    
    type ImageDimension with
        member x.GetTarget(samples : int, isArray : bool) =
            match x with
            | ImageDimension.Image1d _ ->
                if samples > 1 then failwith "[GL] cannot handle 1d multisample textures"

                if isArray then 
                    { 
                        target = TextureTarget.Texture1DArray
                        targets = [ TextureTarget.Texture1DArray ]
                        imageDimension = 2
                        storeDimension = 2
                        array = true
                        multisampled = false
                    }
                else 
                    { 
                        target = TextureTarget.Texture1D
                        targets = [ TextureTarget.Texture1D ]
                        imageDimension = 1
                        storeDimension = 1
                        array = false
                        multisampled = false
                    }

            | ImageDimension.Image2d _ ->
                if isArray then
                    if samples > 1 then
                        { 
                            target = TextureTarget.Texture2DMultisampleArray
                            targets = [ TextureTarget.Texture2DMultisampleArray ]
                            storeDimension = 3
                            imageDimension = 3
                            array = true
                            multisampled = true
                        }
                    else 
                        { 
                            target = TextureTarget.Texture2DArray
                            targets = [ TextureTarget.Texture2DArray ]
                            storeDimension = 3
                            imageDimension = 3
                            array = true
                            multisampled = false
                        }
                else
                    if samples > 1 then
                        { 
                            target = TextureTarget.Texture2DMultisample
                            targets = [ TextureTarget.Texture2DMultisample ]
                            storeDimension = 2
                            imageDimension = 2
                            array = false
                            multisampled = true
                        }
                    else 
                        { 
                            target = TextureTarget.Texture2D
                            targets = [ TextureTarget.Texture2D ]
                            storeDimension = 2
                            imageDimension = 2
                            array = false
                            multisampled = false
                        }
   
            | ImageDimension.Image3d _ ->         
                if isArray then failwith "[GL] cannot handle 3d texture arrays"
                if samples > 1 then failwith "[GL] cannot handle 3d multisampled textures"

                { 
                    target = TextureTarget.Texture3D
                    targets = [ TextureTarget.Texture3D ]
                    storeDimension = 3
                    imageDimension = 3
                    array = false
                    multisampled = false
                }
                
            | ImageDimension.ImageCube _ ->
                if isArray then
                    { 
                        target = TextureTarget.TextureCubeMapArray
                        targets = [ TextureTarget.TextureCubeMapArray ]
                        storeDimension = 3
                        imageDimension = 3
                        array = true
                        multisampled = samples > 1
                    }
                else 
                    { 
                        target = TextureTarget.TextureCubeMap
                        targets = 
                            [ 
                                TextureTarget.TextureCubeMapPositiveX
                                TextureTarget.TextureCubeMapNegativeX
                                TextureTarget.TextureCubeMapPositiveY
                                TextureTarget.TextureCubeMapNegativeY
                                TextureTarget.TextureCubeMapPositiveZ
                                TextureTarget.TextureCubeMapNegativeZ
                            ]
                        imageDimension = 2
                        storeDimension = 2
                        array = false
                        multisampled = samples > 1
                    }

    type Col.Format with
        member x.PixelFormat =
            match x with
            | Col.Format.Gray -> PixelFormat.Red
            | Col.Format.NormalUV -> PixelFormat.RG
            | Col.Format.RGB -> PixelFormat.Rgb
            | Col.Format.RGBA -> PixelFormat.Rgba
            | Col.Format.BGR -> PixelFormat.Bgr
            | Col.Format.BGRA -> PixelFormat.Bgra
            | Col.Format.RGBP -> PixelFormat.Rgba
            | _ -> failwithf "[GL] unknown col.format: %A" x

        member x.Channels =
            match x with
            | Col.Format.Gray -> 1
            | Col.Format.NormalUV -> 2
            | Col.Format.RGB -> 3
            | Col.Format.RGBA -> 4
            | Col.Format.BGR -> 3
            | Col.Format.BGRA -> 4
            | Col.Format.RGBP -> 4
            | _ -> failwithf "[GL] unknown col.format: %A" x


    type ImageFormat with
        member x.PixelFormat =
            match ImageFormat.colorFormat x with
            | ColorFormat.Gray -> PixelFormat.Red
            | ColorFormat.Rg -> PixelFormat.RG
            | ColorFormat.SRgb -> PixelFormat.Rgb
            | ColorFormat.SRgba -> PixelFormat.Rgba
            | ColorFormat.Rgb -> PixelFormat.Rgb
            | ColorFormat.Rgba -> PixelFormat.Rgba
            | ColorFormat.Depth -> PixelFormat.DepthComponent
            | ColorFormat.DepthStencil -> PixelFormat.DepthComponent
            | _ -> PixelFormat.Red

    type Type with
        member x.PixelType =
            if x = typeof<uint8> then PixelType.UnsignedByte
            elif x = typeof<int8> then PixelType.Byte
            elif x = typeof<uint16> then PixelType.UnsignedShort
            elif x = typeof<int16> then PixelType.Short
            elif x = typeof<uint32> then PixelType.UnsignedInt
            elif x = typeof<int32> then PixelType.Int
            elif x = typeof<float32> then PixelType.Float
            elif x = typeof<Depth24Stencil8> then PixelType.UnsignedInt
            else failwithf "[GL] unknown pixeltype: %A" x

    type Image with
        member x.Target = x.Dimension.GetTarget(x.Samples, x.IsArray)

        member x.StorageSize = 
            x.Dimension.GetStorageSize x.Slices

    type ImageSubresourceLevel with
        member x.StorageSize = 
            let s = x.Image.Dimension / (1 <<< x.Level)
            s.GetStorageSize x.Slices



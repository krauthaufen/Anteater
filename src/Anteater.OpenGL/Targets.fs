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
                                TextureTarget.TextureCubeMapNegativeX
                                TextureTarget.TextureCubeMapPositiveX
                                TextureTarget.TextureCubeMapNegativeY
                                TextureTarget.TextureCubeMapPositiveY
                                TextureTarget.TextureCubeMapNegativeZ
                                TextureTarget.TextureCubeMapPositiveZ
                            ]
                        imageDimension = 2
                        storeDimension = 2
                        array = false
                        multisampled = samples > 1
                    }

        

    type Image with
        member x.Target = x.Dimension.GetTarget(x.Samples, x.IsArray)

        member x.StorageSize = 
            x.Dimension.GetStorageSize x.Slices

    type ImageSubresourceLevel with
        member x.StorageSize = 
            let s = x.Image.Dimension / (1 <<< x.Level)
            s.GetStorageSize x.Slices



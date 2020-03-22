namespace rec Anteater

open System
open Aardvark.Base
open Anteater
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"


[<RequireQualifiedAccess>]
type ImageKind =
    | Image1d
    | Image2d
    | Image3d
    | ImageCube

[<RequireQualifiedAccess>]
type ImageDimension =
    | Image1d of size : int
    | Image2d of size : V2i
    | Image3d of size : V3i
    | ImageCube of size : int

    member x.Kind =
        match x with
        | ImageDimension.Image1d _ -> ImageKind.Image1d
        | ImageDimension.Image2d _ -> ImageKind.Image2d
        | ImageDimension.Image3d _ -> ImageKind.Image3d
        | ImageDimension.ImageCube _ -> ImageKind.ImageCube

    member x.GetImageSize(count : int) =
        match x with
        | ImageDimension.Image1d s -> V3i(s, count, 1)
        | ImageDimension.Image2d s -> V3i(s, count)
        | ImageDimension.Image3d s -> s
        | ImageDimension.ImageCube s -> 
            if count > 1 then V3i(s, s, count * 6)
            else V3i(s, s, 1)
       
    member x.GetStorageSize(count : int) =
        match x with
        | ImageDimension.Image1d s -> V3i(s, count, 1)
        | ImageDimension.Image2d s -> V3i(s, count)
        | ImageDimension.Image3d s -> s
        | ImageDimension.ImageCube s -> 
            if count > 1 then V3i(s, s, count * 6)
            else V3i(s,s,1)
       
    static member (/) (s : ImageDimension, d : int) =
        match s with
        | Image1d s -> Image1d ((s / d) |> max 1)
        | Image2d s -> Image2d ((s / d) |> max 1)
        | Image3d s -> Image3d ((s / d) |> max 1)
        | ImageCube s -> ImageCube ((s / d) |> max 1)

    static member (*) (s : ImageDimension, d : int) =
        match s with
        | Image1d s -> Image1d ((s * d) |> max 1)
        | Image2d s -> Image2d ((s * d) |> max 1)
        | Image3d s -> Image3d ((s * d) |> max 1)
        | ImageCube s -> ImageCube ((s * d) |> max 1)

[<RequireQualifiedAccess>]
type ColorFormat =
    | Gray          = 1
    | Rg            = 2
    | SRgb          = 3
    | SRgba         = 4
    | Rgb           = 5
    | Rgba          = 6
    | Depth         = 7
    | DepthStencil  = 8

[<RequireQualifiedAccess>]
type ImageFormat =
    // single channel
    | R8UNorm       = 33321
    | R8SNorm       = 36756
    | R8Int         = 33329
    | R8UInt        = 33330
    | R16UNorm      = 33322
    | R16SNorm      = 36760
    | R16Float      = 33325
    | R16Int        = 33331
    | R16UInt       = 33332
    | R32Float      = 33326
    | R32Int        = 33333
    | R32UInt       = 33334
    
    // dual channel
    | Rg8UNorm      = 33323
    | Rg8SNorm      = 36757
    | Rg8Int        = 33335
    | Rg8UInt       = 33336
    | Rg16UNorm     = 33324
    | Rg16SNorm     = 36761
    | Rg16Float     = 33327
    | Rg16Int       = 33337
    | Rg16UInt      = 33338
    | Rg32Float     = 33328
    | Rg32Int       = 33339
    | Rg32UInt      = 33340

    // three channel
    | Rgb8UNorm     = 32849
    | Rgb8SNorm     = 36758
    | Rgb8Int       = 36239
    | Rgb8UInt      = 36221
    | Rgb16UNorm    = 32852
    | Rgb16SNorm    = 36762
    | Rgb16Float    = 34843
    | Rgb16Int      = 36233
    | Rgb16UInt     = 36215
    | Rgb32Float    = 34837
    | Rgb32Int      = 36227
    | Rgb32UInt     = 36209

    // four channel
    | Rgba8UNorm    = 32856
    | Rgba8SNorm    = 36759
    | Rgba8Int      = 36238
    | Rgba8UInt     = 36220
    | Rgba16UNorm   = 32859
    | Rgba16SNorm   = 36763
    | Rgba16Float   = 34842
    | Rgba16Int     = 36232
    | Rgba16UInt    = 36214
    | Rgba32Float   = 34836
    | Rgba32Int     = 36226
    | Rgba32UInt    = 36208
    
    // srgb
    | SRgb8Unorm    = 35905
    | SRgba8Unorm   = 35907

    // depth formats
    | Depth16           = 33189
    | Depth24           = 33190
    | Depth32           = 33191
    | Depth32f          = 36012
    | Depth32fStencil8  = 36013
    | Depth24Stencil8   = 35056

module ImageFormat =

    let private lookupTable entries = 
        let dict = System.Collections.Generic.Dictionary<'a, 'b>()
        for (k,v) in entries do
            dict.[k] <- v

        fun a -> dict.[a]

    let texelSizeInBytes = 
        lookupTable [
            // single channel
            ImageFormat.R8UNorm, 1
            ImageFormat.R8SNorm, 1
            ImageFormat.R8Int, 1
            ImageFormat.R8UInt, 1
            ImageFormat.R16UNorm, 2
            ImageFormat.R16SNorm, 2
            ImageFormat.R16Float, 2
            ImageFormat.R16Int, 2
            ImageFormat.R16UInt, 2
            ImageFormat.R32Float, 4
            ImageFormat.R32Int, 4
            ImageFormat.R32UInt, 4
    
            // dual channel
            ImageFormat.Rg8UNorm, 2
            ImageFormat.Rg8SNorm, 2
            ImageFormat.Rg8Int, 2
            ImageFormat.Rg8UInt, 2
            ImageFormat.Rg16UNorm, 4
            ImageFormat.Rg16SNorm, 4
            ImageFormat.Rg16Float, 4
            ImageFormat.Rg16Int, 4
            ImageFormat.Rg16UInt, 4
            ImageFormat.Rg32Float, 8
            ImageFormat.Rg32Int, 8
            ImageFormat.Rg32UInt, 8

            // three channel
            ImageFormat.Rgb8UNorm, 3
            ImageFormat.Rgb8SNorm, 3
            ImageFormat.Rgb8Int, 3
            ImageFormat.Rgb8UInt, 3
            ImageFormat.Rgb16UNorm, 6
            ImageFormat.Rgb16SNorm, 6
            ImageFormat.Rgb16Float, 6
            ImageFormat.Rgb16Int, 6
            ImageFormat.Rgb16UInt, 6
            ImageFormat.Rgb32Float, 12
            ImageFormat.Rgb32Int, 12
            ImageFormat.Rgb32UInt, 12

            // four channel
            ImageFormat.Rgba8UNorm, 4
            ImageFormat.Rgba8SNorm, 4
            ImageFormat.Rgba8Int, 4
            ImageFormat.Rgba8UInt, 4
            ImageFormat.Rgba16UNorm, 8
            ImageFormat.Rgba16SNorm, 8
            ImageFormat.Rgba16Float, 8
            ImageFormat.Rgba16Int, 8
            ImageFormat.Rgba16UInt, 8
            ImageFormat.Rgba32Float, 16
            ImageFormat.Rgba32Int, 16
            ImageFormat.Rgba32UInt, 16
    
            // srgb
            ImageFormat.SRgb8Unorm, 3
            ImageFormat.SRgba8Unorm, 4

            // depth formats
            ImageFormat.Depth16, 2
            ImageFormat.Depth24, 3
            ImageFormat.Depth32, 4
            ImageFormat.Depth32f, 4
            ImageFormat.Depth32fStencil8, 5
            ImageFormat.Depth24Stencil8, 4
        ]

    let isInteger = 
        lookupTable [
            // single channel
            ImageFormat.R8UNorm,        false
            ImageFormat.R8SNorm,        false
            ImageFormat.R8Int,          true
            ImageFormat.R8UInt,         true
            ImageFormat.R16UNorm,       false
            ImageFormat.R16SNorm,       false
            ImageFormat.R16Float,       false
            ImageFormat.R16Int,         true
            ImageFormat.R16UInt,        true
            ImageFormat.R32Float,       false
            ImageFormat.R32Int,         true
            ImageFormat.R32UInt,        true
    
            // dual channel
            ImageFormat.Rg8UNorm,       false
            ImageFormat.Rg8SNorm,       false
            ImageFormat.Rg8Int,         true
            ImageFormat.Rg8UInt,        true
            ImageFormat.Rg16UNorm,      false
            ImageFormat.Rg16SNorm,      false
            ImageFormat.Rg16Float,      false
            ImageFormat.Rg16Int,        true
            ImageFormat.Rg16UInt,       true
            ImageFormat.Rg32Float,      false
            ImageFormat.Rg32Int,        true
            ImageFormat.Rg32UInt,       true

            // three channel
            ImageFormat.Rgb8UNorm,      false
            ImageFormat.Rgb8SNorm,      false
            ImageFormat.Rgb8Int,        true
            ImageFormat.Rgb8UInt,       true
            ImageFormat.Rgb16UNorm,     false
            ImageFormat.Rgb16SNorm,     false
            ImageFormat.Rgb16Float,     false
            ImageFormat.Rgb16Int,       true
            ImageFormat.Rgb16UInt,      true
            ImageFormat.Rgb32Float,     false
            ImageFormat.Rgb32Int,       true
            ImageFormat.Rgb32UInt,      true

            // four channel
            ImageFormat.Rgba8UNorm,     false
            ImageFormat.Rgba8SNorm,     false
            ImageFormat.Rgba8Int,       true
            ImageFormat.Rgba8UInt,      true
            ImageFormat.Rgba16UNorm,    false
            ImageFormat.Rgba16SNorm,    false
            ImageFormat.Rgba16Float,    false
            ImageFormat.Rgba16Int,      true
            ImageFormat.Rgba16UInt,     true
            ImageFormat.Rgba32Float,    false
            ImageFormat.Rgba32Int,      true
            ImageFormat.Rgba32UInt,     true
    
            // srgb
            ImageFormat.SRgb8Unorm,     false
            ImageFormat.SRgba8Unorm,    false

            // depth formats
            ImageFormat.Depth16,        false
            ImageFormat.Depth24,        false
            ImageFormat.Depth32,        false
            ImageFormat.Depth32f,       false
            ImageFormat.Depth32fStencil8, false
            ImageFormat.Depth24Stencil8, false
        ]
        
    let isSigned = 
        lookupTable [
            // single channel
            ImageFormat.R8UNorm,        false
            ImageFormat.R8SNorm,        true
            ImageFormat.R8Int,          true
            ImageFormat.R8UInt,         false
            ImageFormat.R16UNorm,       false
            ImageFormat.R16SNorm,       true
            ImageFormat.R16Float,       true
            ImageFormat.R16Int,         true
            ImageFormat.R16UInt,        false
            ImageFormat.R32Float,       true
            ImageFormat.R32Int,         true
            ImageFormat.R32UInt,        false
    
            // dual channel
            ImageFormat.Rg8UNorm,       false
            ImageFormat.Rg8SNorm,       true
            ImageFormat.Rg8Int,         true
            ImageFormat.Rg8UInt,        false
            ImageFormat.Rg16UNorm,      false
            ImageFormat.Rg16SNorm,      true
            ImageFormat.Rg16Float,      true
            ImageFormat.Rg16Int,        true
            ImageFormat.Rg16UInt,       false
            ImageFormat.Rg32Float,      true
            ImageFormat.Rg32Int,        true
            ImageFormat.Rg32UInt,       false

            // three channel
            ImageFormat.Rgb8UNorm,      false
            ImageFormat.Rgb8SNorm,      true
            ImageFormat.Rgb8Int,        true
            ImageFormat.Rgb8UInt,       false
            ImageFormat.Rgb16UNorm,     false
            ImageFormat.Rgb16SNorm,     true
            ImageFormat.Rgb16Float,     true
            ImageFormat.Rgb16Int,       true
            ImageFormat.Rgb16UInt,      false
            ImageFormat.Rgb32Float,     true
            ImageFormat.Rgb32Int,       true
            ImageFormat.Rgb32UInt,      false

            // four channel
            ImageFormat.Rgba8UNorm,     false
            ImageFormat.Rgba8SNorm,     true
            ImageFormat.Rgba8Int,       true
            ImageFormat.Rgba8UInt,      false
            ImageFormat.Rgba16UNorm,    false
            ImageFormat.Rgba16SNorm,    true
            ImageFormat.Rgba16Float,    true
            ImageFormat.Rgba16Int,      true
            ImageFormat.Rgba16UInt,     false
            ImageFormat.Rgba32Float,    true
            ImageFormat.Rgba32Int,      true
            ImageFormat.Rgba32UInt,     false
    
            // srgb
            ImageFormat.SRgb8Unorm,     false
            ImageFormat.SRgba8Unorm,    false

            // depth formats
            ImageFormat.Depth16,        false
            ImageFormat.Depth24,        false
            ImageFormat.Depth32,        false
            ImageFormat.Depth32f,       false
            ImageFormat.Depth32fStencil8, false
            ImageFormat.Depth24Stencil8, false
        ]

    let colorFormat = 
        lookupTable [
            // single channel
            ImageFormat.R8UNorm, ColorFormat.Gray
            ImageFormat.R8SNorm, ColorFormat.Gray
            ImageFormat.R8Int, ColorFormat.Gray
            ImageFormat.R8UInt, ColorFormat.Gray
            ImageFormat.R16UNorm, ColorFormat.Gray
            ImageFormat.R16SNorm, ColorFormat.Gray
            ImageFormat.R16Float, ColorFormat.Gray
            ImageFormat.R16Int, ColorFormat.Gray
            ImageFormat.R16UInt, ColorFormat.Gray
            ImageFormat.R32Float, ColorFormat.Gray
            ImageFormat.R32Int, ColorFormat.Gray
            ImageFormat.R32UInt, ColorFormat.Gray
    
            // dual channel
            ImageFormat.Rg8UNorm, ColorFormat.Rg
            ImageFormat.Rg8SNorm, ColorFormat.Rg
            ImageFormat.Rg8Int, ColorFormat.Rg
            ImageFormat.Rg8UInt, ColorFormat.Rg
            ImageFormat.Rg16UNorm, ColorFormat.Rg
            ImageFormat.Rg16SNorm, ColorFormat.Rg
            ImageFormat.Rg16Float, ColorFormat.Rg
            ImageFormat.Rg16Int, ColorFormat.Rg
            ImageFormat.Rg16UInt, ColorFormat.Rg
            ImageFormat.Rg32Float, ColorFormat.Rg
            ImageFormat.Rg32Int, ColorFormat.Rg
            ImageFormat.Rg32UInt, ColorFormat.Rg

            // three channel
            ImageFormat.Rgb8UNorm, ColorFormat.Rgb
            ImageFormat.Rgb8SNorm, ColorFormat.Rgb
            ImageFormat.Rgb8Int, ColorFormat.Rgb
            ImageFormat.Rgb8UInt, ColorFormat.Rgb
            ImageFormat.Rgb16UNorm, ColorFormat.Rgb
            ImageFormat.Rgb16SNorm, ColorFormat.Rgb
            ImageFormat.Rgb16Float, ColorFormat.Rgb
            ImageFormat.Rgb16Int, ColorFormat.Rgb
            ImageFormat.Rgb16UInt, ColorFormat.Rgb
            ImageFormat.Rgb32Float, ColorFormat.Rgb
            ImageFormat.Rgb32Int, ColorFormat.Rgb
            ImageFormat.Rgb32UInt, ColorFormat.Rgb

            // four channel
            ImageFormat.Rgba8UNorm, ColorFormat.Rgba
            ImageFormat.Rgba8SNorm, ColorFormat.Rgba
            ImageFormat.Rgba8Int, ColorFormat.Rgba
            ImageFormat.Rgba8UInt, ColorFormat.Rgba
            ImageFormat.Rgba16UNorm, ColorFormat.Rgba
            ImageFormat.Rgba16SNorm, ColorFormat.Rgba
            ImageFormat.Rgba16Float, ColorFormat.Rgba
            ImageFormat.Rgba16Int, ColorFormat.Rgba
            ImageFormat.Rgba16UInt, ColorFormat.Rgba
            ImageFormat.Rgba32Float, ColorFormat.Rgba
            ImageFormat.Rgba32Int, ColorFormat.Rgba
            ImageFormat.Rgba32UInt, ColorFormat.Rgba
    
            // srgb
            ImageFormat.SRgb8Unorm, ColorFormat.SRgb
            ImageFormat.SRgba8Unorm, ColorFormat.SRgba

            // depth formats
            ImageFormat.Depth16, ColorFormat.Depth
            ImageFormat.Depth24, ColorFormat.Depth
            ImageFormat.Depth32, ColorFormat.Depth
            ImageFormat.Depth32f, ColorFormat.Depth
            ImageFormat.Depth32fStencil8, ColorFormat.DepthStencil
            ImageFormat.Depth24Stencil8, ColorFormat.DepthStencil
        ]
        
    let channels = 
        lookupTable [
            // single channel
            ImageFormat.R8UNorm,        1
            ImageFormat.R8SNorm,        1
            ImageFormat.R8Int,          1
            ImageFormat.R8UInt,         1
            ImageFormat.R16UNorm,       1
            ImageFormat.R16SNorm,       1
            ImageFormat.R16Float,       1
            ImageFormat.R16Int,         1
            ImageFormat.R16UInt,        1
            ImageFormat.R32Float,       1
            ImageFormat.R32Int,         1
            ImageFormat.R32UInt,        1
    
            // dual channel
            ImageFormat.Rg8UNorm,       2
            ImageFormat.Rg8SNorm,       2
            ImageFormat.Rg8Int,         2
            ImageFormat.Rg8UInt,        2
            ImageFormat.Rg16UNorm,      2
            ImageFormat.Rg16SNorm,      2
            ImageFormat.Rg16Float,      2
            ImageFormat.Rg16Int,        2
            ImageFormat.Rg16UInt,       2
            ImageFormat.Rg32Float,      2
            ImageFormat.Rg32Int,        2
            ImageFormat.Rg32UInt,       2

            // three channel
            ImageFormat.Rgb8UNorm,      3
            ImageFormat.Rgb8SNorm,      3
            ImageFormat.Rgb8Int,        3
            ImageFormat.Rgb8UInt,       3
            ImageFormat.Rgb16UNorm,     3
            ImageFormat.Rgb16SNorm,     3
            ImageFormat.Rgb16Float,     3
            ImageFormat.Rgb16Int,       3
            ImageFormat.Rgb16UInt,      3
            ImageFormat.Rgb32Float,     3
            ImageFormat.Rgb32Int,       3
            ImageFormat.Rgb32UInt,      3

            // four channel
            ImageFormat.Rgba8UNorm,     4
            ImageFormat.Rgba8SNorm,     4
            ImageFormat.Rgba8Int,       4
            ImageFormat.Rgba8UInt,      4
            ImageFormat.Rgba16UNorm,    4
            ImageFormat.Rgba16SNorm,    4
            ImageFormat.Rgba16Float,    4
            ImageFormat.Rgba16Int,      4
            ImageFormat.Rgba16UInt,     4
            ImageFormat.Rgba32Float,    4
            ImageFormat.Rgba32Int,      4
            ImageFormat.Rgba32UInt,     4
    
            // srgb
            ImageFormat.SRgb8Unorm,     3
            ImageFormat.SRgba8Unorm,    4

            // depth formats
            ImageFormat.Depth16,            1
            ImageFormat.Depth24,            1
            ImageFormat.Depth32,            1
            ImageFormat.Depth32f,           1
            ImageFormat.Depth32fStencil8,   1
            ImageFormat.Depth24Stencil8,    1
        ]
        
    let hasDepth (fmt : ImageFormat) =
        match fmt with
        | ImageFormat.Depth16
        | ImageFormat.Depth24
        | ImageFormat.Depth32
        | ImageFormat.Depth32f
        | ImageFormat.Depth32fStencil8
        | ImageFormat.Depth24Stencil8 ->
            true
        | _ ->
            false
    let hasStencil (fmt : ImageFormat) =
        match fmt with
        | ImageFormat.Depth32fStencil8
        | ImageFormat.Depth24Stencil8 ->
            true
        | _ ->
            false

    let channelTypes = 
        lookupTable [
            // single channel
            ImageFormat.R8UNorm,        HashSet.ofList [ typeof<uint8> ]
            ImageFormat.R8SNorm,        HashSet.ofList [ typeof<int8> ]
            ImageFormat.R8Int,          HashSet.ofList [ typeof<int8> ]
            ImageFormat.R8UInt,         HashSet.ofList [ typeof<uint8> ]
            ImageFormat.R16UNorm,       HashSet.ofList [ typeof<uint16> ]
            ImageFormat.R16SNorm,       HashSet.ofList [ typeof<int16> ]
            ImageFormat.R16Float,       HashSet.ofList [ typeof<float16> ]
            ImageFormat.R16Int,         HashSet.ofList [ typeof<int16> ]
            ImageFormat.R16UInt,        HashSet.ofList [ typeof<uint16> ]
            ImageFormat.R32Float,       HashSet.ofList [ typeof<float32> ]
            ImageFormat.R32Int,         HashSet.ofList [ typeof<int32> ]
            ImageFormat.R32UInt,        HashSet.ofList [ typeof<uint32> ]
    
            // dual channel
            ImageFormat.Rg8UNorm,       HashSet.ofList [ typeof<uint8> ]
            ImageFormat.Rg8SNorm,       HashSet.ofList [ typeof<int8> ]
            ImageFormat.Rg8Int,         HashSet.ofList [ typeof<int8> ]
            ImageFormat.Rg8UInt,        HashSet.ofList [ typeof<uint8> ]
            ImageFormat.Rg16UNorm,      HashSet.ofList [ typeof<uint16> ]
            ImageFormat.Rg16SNorm,      HashSet.ofList [ typeof<int16> ]
            ImageFormat.Rg16Float,      HashSet.ofList [ typeof<float16> ]
            ImageFormat.Rg16Int,        HashSet.ofList [ typeof<int16> ]
            ImageFormat.Rg16UInt,       HashSet.ofList [ typeof<uint16> ]
            ImageFormat.Rg32Float,      HashSet.ofList [ typeof<float32> ]
            ImageFormat.Rg32Int,        HashSet.ofList [ typeof<int32> ]
            ImageFormat.Rg32UInt,       HashSet.ofList [ typeof<uint32> ]

            // three channel
            ImageFormat.Rgb8UNorm,      HashSet.ofList [ typeof<uint8> ]
            ImageFormat.Rgb8SNorm,      HashSet.ofList [ typeof<int8> ]
            ImageFormat.Rgb8Int,        HashSet.ofList [ typeof<int8> ]
            ImageFormat.Rgb8UInt,       HashSet.ofList [ typeof<uint8> ]
            ImageFormat.Rgb16UNorm,     HashSet.ofList [ typeof<uint16> ]
            ImageFormat.Rgb16SNorm,     HashSet.ofList [ typeof<int16> ]
            ImageFormat.Rgb16Float,     HashSet.ofList [ typeof<float16> ]
            ImageFormat.Rgb16Int,       HashSet.ofList [ typeof<int16> ]
            ImageFormat.Rgb16UInt,      HashSet.ofList [ typeof<uint16> ]
            ImageFormat.Rgb32Float,     HashSet.ofList [ typeof<float32> ]
            ImageFormat.Rgb32Int,       HashSet.ofList [ typeof<int32> ]
            ImageFormat.Rgb32UInt,      HashSet.ofList [ typeof<uint32> ]

            // four channel
            ImageFormat.Rgba8UNorm,     HashSet.ofList [ typeof<uint8> ]
            ImageFormat.Rgba8SNorm,     HashSet.ofList [ typeof<int8> ]
            ImageFormat.Rgba8Int,       HashSet.ofList [ typeof<int8> ]
            ImageFormat.Rgba8UInt,      HashSet.ofList [ typeof<uint8> ]
            ImageFormat.Rgba16UNorm,    HashSet.ofList [ typeof<uint16> ]
            ImageFormat.Rgba16SNorm,    HashSet.ofList [ typeof<int16> ]
            ImageFormat.Rgba16Float,    HashSet.ofList [ typeof<float16> ]
            ImageFormat.Rgba16Int,      HashSet.ofList [ typeof<int16> ]
            ImageFormat.Rgba16UInt,     HashSet.ofList [ typeof<uint16> ]
            ImageFormat.Rgba32Float,    HashSet.ofList [ typeof<float32> ]
            ImageFormat.Rgba32Int,      HashSet.ofList [ typeof<int32> ]
            ImageFormat.Rgba32UInt,     HashSet.ofList [ typeof<uint32> ]
    
            // srgb
            ImageFormat.SRgb8Unorm,     HashSet.ofList [ typeof<uint8> ]
            ImageFormat.SRgba8Unorm,    HashSet.ofList [ typeof<uint8> ]

            // depth formats
            ImageFormat.Depth16,            HashSet.ofList [ typeof<float16> ]
            ImageFormat.Depth24,            HashSet.ofList [ typeof<float32>; typeof<uint32>; typeof<uint24> ]
            ImageFormat.Depth32,            HashSet.ofList [ typeof<float32>; typeof<uint32> ]
            ImageFormat.Depth32f,           HashSet.ofList [ typeof<float32> ]
            ImageFormat.Depth32fStencil8,   HashSet.ofList [ typeof<float32> ]
            ImageFormat.Depth24Stencil8,    HashSet.ofList [ typeof<float32>; typeof<uint32>; typeof<Depth24Stencil8> ]
        ]


type ImageSubresourceRange =
    abstract member Image : Image
    abstract member BaseLevel : int
    abstract member Levels : int
    abstract member BaseSlice : int
    abstract member Slices : int

    abstract member Item : level : int -> ImageSubresourceLevel with get
    abstract member Item : level : int * slice : int -> ImageSubresource with get

    abstract member GetSlice : minLevel : option<int> * maxLevel : option<int> * minSlice : option<int> * maxSlice : option<int> -> ImageSubresourceRange
    abstract member GetSlice : level : int * minSlice : option<int> * maxSlice : option<int> -> ImageSubresourceLevel
    abstract member GetSlice : minLevel : option<int> * maxLevel : option<int> * slice : int -> ImageSubresourceSlice

type ImageSubresourceLevel = 
    inherit ImageSubresourceRange
    inherit ImageSubresourceRegion
    abstract member Level : int
    abstract member Size : V3i
    abstract member Item : slice : int -> ImageSubresource with get
    abstract member GetSlice : minSlice : option<int> * maxSlice : option<int> -> ImageSubresourceLevel
    
type ImageSubresourceSlice = 
    inherit ImageSubresourceRange
    abstract member Slice : int
    abstract member Item : level : int -> ImageSubresource with get
    abstract member GetSlice : minLevel : option<int> * maxLevel : option<int> -> ImageSubresourceSlice

type ImageSubresource = 
    inherit ImageSubresourceLevel
    inherit ImageSubresourceSlice
    
type ImageSubresourceRegion =
    abstract member Resource : ImageSubresourceLevel
    abstract member Offset : V3i
    abstract member Size : V3i
    

type Image(handle : obj, dimension : ImageDimension, format : ImageFormat, levels : int, slices : option<int>, samples : int, release : obj -> unit) =
    let mutable handle = handle

    let sliceCount = defaultArg slices 1

    member x.Handle = handle
    member x.Size = dimension.GetImageSize 1
    
    member x.Dimension = dimension
    member x.Format = format
    member x.Levels = levels
    member x.Slices = sliceCount
    member x.Samples = samples
    member x.IsArray = Option.isSome slices
    
    /// Deletes the Buffer.
    member x.Dispose() =
        if not (isNull handle) then
            release handle
            handle <- null

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface ImageSubresourceRange with
        member x.Image = x
        member x.BaseLevel = 0
        member x.Levels = x.Levels
        member x.BaseSlice = 0
        member x.Slices = x.Slices

        member x.Item 
            with get(level : int) = ImgSubresourceLevel(x, level, 0, x.Slices) :> ImageSubresourceLevel

        member x.Item 
            with get(level : int, slice : int) = ImgSubresource(x, level, slice) :> ImageSubresource

        member x.GetSlice(minLevel : option<int>, maxLevel : option<int>, minSlice : option<int>, maxSlice : option<int>) =
            let self = ImgSubresourceRange(x, 0, x.Levels, 0, x.Slices) :> ImageSubresourceRange
            self.GetSlice(minLevel, maxLevel, minSlice, maxSlice)
            
        member x.GetSlice(level : int, minSlice : option<int>, maxSlice : option<int>) =
            let self = ImgSubresourceRange(x, 0, x.Levels, 0, x.Slices) :> ImageSubresourceRange
            self.GetSlice(level, minSlice, maxSlice)
            
        member x.GetSlice(minLevel : option<int>, maxLevel : option<int>, slice : int) =
            let self = ImgSubresourceRange(x, 0, x.Levels, 0, x.Slices) :> ImageSubresourceRange
            self.GetSlice(minLevel, maxLevel, slice)

    member x.GetSubresource(?level : int, ?slice : int) =
        let level = defaultArg level 0
        let slice = defaultArg slice 0
        ImgSubresource(x, level, slice) :> ImageSubresource


    member x.Item
        with get(level : int) = 
            let slices = defaultArg slices 1
            if level < 0 || level >= levels then raise <| IndexOutOfRangeException()
            ImgSubresourceLevel(x, level, 0, slices) :> ImageSubresourceLevel

    member x.Item
        with get(level : int, slice : int) = 
            let slices = defaultArg slices 1
            if level < 0 || level >= levels then raise <| IndexOutOfRangeException()
            if slice < 0 || slice >= slices then raise <| IndexOutOfRangeException()
            ImgSubresource(x, level, slice) :> ImageSubresource


    member x.GetSlice(minLevel : option<int>, maxLevel : option<int>, minSlice : option<int>, maxSlice : option<int>) =
        let self = ImgSubresourceRange(x, 0, x.Levels, 0, x.Slices) :> ImageSubresourceRange
        self.GetSlice(minLevel, maxLevel, minSlice, maxSlice)
            
    member x.GetSlice(level : int, minSlice : option<int>, maxSlice : option<int>) =
        let self = ImgSubresourceRange(x, 0, x.Levels, 0, x.Slices) :> ImageSubresourceRange
        self.GetSlice(level, minSlice, maxSlice)
            
    member x.GetSlice(minLevel : option<int>, maxLevel : option<int>, slice : int) =
        let self = ImgSubresourceRange(x, 0, x.Levels, 0, x.Slices) :> ImageSubresourceRange
        self.GetSlice(minLevel, maxLevel, slice)
    

[<AutoOpen>]
module ImageSubresourceImplementations =
    
    let private _sliceTest (img : Image) =
        // Image.Item
        let _ : ImageSubresourceLevel   = img.[0]
        let _ : ImageSubresource        = img.[0, 0]

        // Image.GetSlice
        let r : ImageSubresourceRange   = img.[*, *]
        let _ : ImageSubresourceLevel   = img.[1, *]
        let s : ImageSubresourceSlice   = img.[*, 4]

        // ImageSubResourceRange.Item
        let l : ImageSubresourceLevel   = r.[0]
        let _ : ImageSubresource        = r.[0, 0]
        
        // ImageSubResourceRange.GetSlice
        let _ : ImageSubresourceRange   = r.[*, *]
        let _ : ImageSubresourceLevel   = r.[0, *]
        let _ : ImageSubresourceSlice   = r.[*, 0]

        // ImageSubResourceLevel.Item
        let _ : ImageSubresource        = l.[0]

        // ImageSubResourceLevel.GetSlice
        let _ : ImageSubresourceLevel   = l.[0..]

        // ImageSubResourceSlice.Item
        let _ : ImageSubresource        = s.[0]

        // ImageSubResourceSlice.GetSlice
        let _ : ImageSubresourceSlice   = s.[*]

        ()

    type internal ImgSubresourceRange(img : Image, l0 : int, levels : int, s0 : int, slices : int) =
        do
            if l0 < 0 || levels < 0 || l0 + levels > img.Levels then raise <| IndexOutOfRangeException "Level"
            if s0 < 0 || slices < 0 || s0 + slices > img.Slices then raise <| IndexOutOfRangeException "Slice"

        interface ImageSubresourceRange with
            member x.Image = img
            member x.BaseLevel = l0
            member x.Levels = levels
            member x.BaseSlice = s0
            member x.Slices = slices
            member x.Item
                with get(level : int) = 
                    if level < 0 || level >= levels then raise <| IndexOutOfRangeException()
                    ImgSubresourceLevel(img, l0 + level, s0, slices) :> ImageSubresourceLevel

            member x.Item
                with get(level : int, slice : int) = 
                    if level < 0 || level >= levels then raise <| IndexOutOfRangeException()
                    if slice < 0 || slice >= slices then raise <| IndexOutOfRangeException()
                    ImgSubresource(img, l0 + level, s0 + slice) :> ImageSubresource

            member x.GetSlice(minLevel : option<int>, maxLevel : option<int>, minSlice : option<int>, maxSlice : option<int>) =
                let minSlice = defaultArg minSlice 0
                let maxSlice = defaultArg maxSlice (slices - 1)
                let sc = 1 + maxSlice - minSlice

                let minLevel = defaultArg minLevel 0
                let maxLevel = defaultArg maxLevel (levels - 1)
                let lc = 1 + maxLevel - minLevel

                if minLevel < 0 || lc < 0 || minLevel + lc > levels then raise <| IndexOutOfRangeException("Level")
                if minSlice < 0 || sc < 0 || minSlice + sc > slices then raise <| IndexOutOfRangeException("Slice")

                ImgSubresourceRange(img, l0 + minLevel, lc, s0 + minSlice, sc) :> ImageSubresourceRange

            member x.GetSlice(level : int, minSlice : option<int>, maxSlice : option<int>) =
                let minSlice = defaultArg minSlice 0
                let maxSlice = defaultArg maxSlice (slices - 1)
                let sc = 1 + maxSlice - minSlice

                if level < 0 || level >= levels then raise <| IndexOutOfRangeException("Level")
                if minSlice < 0 || sc < 0 || minSlice + sc > slices then raise <| IndexOutOfRangeException("Slice")

                ImgSubresourceLevel(img, l0 + level, s0 + minSlice, sc) :> ImageSubresourceLevel
    
            member x.GetSlice(minLevel : option<int>, maxLevel : option<int>, slice : int) =
                let minLevel = defaultArg minLevel 0
                let maxLevel = defaultArg maxLevel (levels - 1)
                let lc = 1 + maxLevel - minLevel

                
                if minLevel < 0 || lc < 0 || minLevel + lc > levels then raise <| IndexOutOfRangeException("Level")
                if slice < 0 || slice >= slices then raise <| IndexOutOfRangeException("Slice")

                ImgSubresourceSlice(img, l0 + minLevel, lc, s0 + slice) :> ImageSubresourceSlice

    type internal ImgSubresourceLevel(img : Image, l0 : int, s0 : int, slices : int) =
        inherit ImgSubresourceRange(img, l0, 1, s0, slices)
        member x.Size =
            let s = img.Size
            let f = 1 <<< l0
            V3i(max 1 (s.X / f), max 1 (s.Y / f), max 1 (s.Z / f))


        interface ImageSubresourceRegion with
            member x.Resource = x :> ImageSubresourceLevel
            member x.Offset = V3i.Zero
            member x.Size = x.Size

        interface ImageSubresourceLevel with
            member x.Level = l0

            member x.Size : V3i = x.Size

            member x.Item
                with get(slice : int) = 
                    if slice < 0 || slice >= slices then raise <| IndexOutOfRangeException()
                    ImgSubresource(img, l0, s0 + slice) :> ImageSubresource
            
            member x.GetSlice(minSlice : option<int>, maxSlice : option<int>) =
                let minSlice = defaultArg minSlice 0
                let maxSlice = defaultArg maxSlice (slices - 1)
                let sc = 1 + maxSlice - minSlice
                if minSlice < 0 || sc < 0 || minSlice + sc > slices then raise <| IndexOutOfRangeException("Slice")
                ImgSubresourceLevel(img, l0, s0 + minSlice, sc) :> ImageSubresourceLevel
    
    type internal ImgSubresourceSlice(img : Image, l0 : int, levels : int, s0 : int) =
        inherit ImgSubresourceRange(img, l0, levels, s0, 1)
        interface ImageSubresourceSlice with
            member x.Slice = s0
            member x.Item
                with get(level : int) = 
                    if level < 0 || level >= levels then raise <| IndexOutOfRangeException()
                    ImgSubresource(img, l0 + level, s0) :> ImageSubresource
                    
            member x.GetSlice(minLevel : option<int>, maxLevel : option<int>) =
                let minLevel = defaultArg minLevel 0
                let maxLevel = defaultArg maxLevel (levels - 1)
                let lc = 1 + maxLevel - minLevel
                if minLevel < 0 || lc < 0 || minLevel + lc > levels then raise <| IndexOutOfRangeException("Level")
                ImgSubresourceSlice(img, l0 + minLevel, lc, s0) :> ImageSubresourceSlice
    
    type internal ImgSubresource(img : Image, level : int, slice : int) =
        inherit ImgSubresourceLevel(img, level, slice, 1)
        interface ImageSubresourceSlice with
            member x.Slice = slice
            member x.Item
                with get(l : int) = 
                    if l <> 0 then raise <| IndexOutOfRangeException()
                    x :> ImageSubresource
                    
            member x.GetSlice(minLevel : option<int>, maxLevel : option<int>) =
                let minLevel = defaultArg minLevel 0
                let maxLevel = defaultArg maxLevel 0
                let lc = 1 + maxLevel - minLevel
                if minLevel < 0 || lc < 0 || minLevel + lc > 1 then raise <| IndexOutOfRangeException("Level")
                x :> ImageSubresourceSlice
    

    
        member x.Sub(offset : V3i, s : V3i) =
            let size = x.Size
            if offset.AnySmaller 0 || s.AnySmaller 0 || (offset + s).AnyGreater size then raise <| IndexOutOfRangeException()

            { new ImageSubresourceRegion with
                member __.Resource = x :> ImageSubresourceLevel
                member __.Offset = offset
                member __.Size = s
            }
        interface ImageSubresource

    type ImageSubresourceLevel with

    
        member x.Sub(offset : V3i, s : V3i) =
            let size = x.Size
            if offset.AnySmaller 0 || s.AnySmaller 0 || (offset + s).AnyGreater size then raise <| IndexOutOfRangeException()

            { new ImageSubresourceRegion with
                member __.Resource = x
                member __.Offset = offset
                member __.Size = s
            }
            
        member x.GetSlice(min : option<V3i>, max : option<V3i>) =
            let size = x.Size
            let min = defaultArg min V3i.Zero
            let max = defaultArg max (size - V3i.III)
            let s = V3i.III + (max - min)
            x.Sub(min, s)

        member x.GetSlice(min : option<V2i>, max : option<V2i>) =
            x.GetSlice(
                min |> Option.map (fun v -> V3i(v, 0)), 
                max |> Option.map (fun v -> V3i(v, 0))
            )

        member x.GetSlice(min : option<int>, max : option<int>) =
            x.GetSlice(
                min |> Option.map (fun v -> V3i(v, 0, 0)), 
                max |> Option.map (fun v -> V3i(v, 0, 0))
            )

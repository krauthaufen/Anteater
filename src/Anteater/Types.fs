namespace Anteater

open System
open Aardvark.Base


[<RequireQualifiedAccess>]
type ImageDimension =
    | Image1d of size : int
    | Image2d of size : V2i
    | Image3d of size : V3i
    | ImageCube of size : int

module ImageDimension =
    let inline size (d : ImageDimension) =
        match d with
        | ImageDimension.Image1d s -> V3i(s,1,1)
        | ImageDimension.Image2d s -> V3i(s, 1)
        | ImageDimension.Image3d s -> s
        | ImageDimension.ImageCube s -> V3i(s,s,1)


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

[<Flags>]
type BufferUsage =
    | None          = 0x000
    | VertexBuffer  = 0x001
    | IndexBuffer   = 0x002
    | StorageBuffer = 0x004
    | CopySrc       = 0x008
    | CopyDst       = 0x010

[<AbstractClass>]
type BufferRange() =
    abstract member Buffer : Buffer
    abstract member Offset : int64
    abstract member Size : int64

    member x.GetSlice(min : option<int64>, max : option<int64>) =
        let min = defaultArg min 0L
        let max = defaultArg max (x.Size - 1L)
        let s = 1L + max - min
        if min < 0L || min >= x.Size || s < 0L || min + s > x.Size then raise <| IndexOutOfRangeException()

        let offset = x.Offset + min
        { new BufferRange() with
            member __.Buffer = x.Buffer
            member __.Offset = offset
            member __.Size = s
        }

    member x.GetSlice(min : option<int>, max : option<int>) =
        x.GetSlice(Option.map int64 min, Option.map int64 max)
        

and Buffer(handle : obj, size : int64, usage : BufferUsage, release : obj -> unit) =
    inherit BufferRange()

    let mutable handle = handle
    let mutable size = size
    let mutable usage = usage

    override x.Offset = 0L
    override x.Buffer = x
    override x.Size = size

    member x.Handle = handle
    member x.Usage = usage
    member x.Dispose() =
        if not (isNull handle) then
            release handle
            handle <- null
            size <- 0L
            usage <- BufferUsage.None

    member x.GetSlice(min : option<int64>, max : option<int64>) =
        let min = defaultArg min 0L
        let max = defaultArg max (size - 1L)
        let s = 1L + max - min
        if min < 0L || min >= size || s < 0L || min + s > size then raise <| IndexOutOfRangeException()
        { new BufferRange() with
            member __.Buffer = x
            member __.Offset = min
            member __.Size = s
        }

    member x.GetSlice(min : option<int>, max : option<int>) =
        x.GetSlice(Option.map int64 min, Option.map int64 max)
        
    interface IDisposable with
        member x.Dispose() = x.Dispose()

[<Struct>]
type Image(handle : obj, dimension : ImageDimension, format : ImageFormat, levels : int, slices : int, samples : int) =
    member x.Handle = handle
    member x.Size = ImageDimension.size dimension
    member x.Dimension = dimension
    member x.Format = format
    member x.Levels = levels
    member x.Slices = slices
    member x.Samples = samples
    

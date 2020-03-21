namespace Anteater

open System
open System.Runtime.CompilerServices
open Aardvark.Base
open Anteater




[<AutoOpen>]
module private PixVisitors =
    [<AbstractClass>]
    type PixImageVisitor<'r>() =
        static let table =
            LookupTable.lookupTable [
                typeof<int8>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<int8>(unbox img, 127y))
                typeof<uint8>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<uint8>(unbox img, 255uy))
                typeof<int16>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<int16>(unbox img, Int16.MaxValue))
                typeof<uint16>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<uint16>(unbox img, UInt16.MaxValue))
                typeof<int32>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<int32>(unbox img, Int32.MaxValue))
                typeof<uint32>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<uint32>(unbox img, UInt32.MaxValue))
                typeof<int64>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<int64>(unbox img, Int64.MaxValue))
                typeof<uint64>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<uint64>(unbox img, UInt64.MaxValue))
                typeof<float16>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<float16>(unbox img, float16(Float32 = 1.0f)))
                typeof<float32>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<float32>(unbox img, 1.0f))
                typeof<float>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<float>(unbox img, 1.0))
            ]
        abstract member Visit<'a when 'a : unmanaged> : PixImage<'a> * 'a -> 'r

        interface IPixImageVisitor<'r> with
            member x.Visit<'a>(img : PixImage<'a>) =
                table (typeof<'a>) (x, img)

    [<AbstractClass>]
    type PixVolumeVisitor<'r>() =
        static let table =
            LookupTable.lookupTable [
                typeof<int8>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<int8>(unbox img, 127y))
                typeof<uint8>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<uint8>(unbox img, 255uy))
                typeof<int16>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<int16>(unbox img, Int16.MaxValue))
                typeof<uint16>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<uint16>(unbox img, UInt16.MaxValue))
                typeof<int32>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<int32>(unbox img, Int32.MaxValue))
                typeof<uint32>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<uint32>(unbox img, UInt32.MaxValue))
                typeof<int64>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<int64>(unbox img, Int64.MaxValue))
                typeof<uint64>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<uint64>(unbox img, UInt64.MaxValue))
                typeof<float16>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<float16>(unbox img, float16(Float32 = 1.0f)))
                typeof<float32>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<float32>(unbox img, 1.0f))
                typeof<float>, (fun (self : PixVolumeVisitor<'r>, img : PixVolume) -> self.Visit<float>(unbox img, 1.0))
            ]

        abstract member Visit<'a when 'a : unmanaged> : PixVolume<'a> * 'a -> 'r

        interface IPixVolumeVisitor<'r> with
            member x.Visit<'a>(img : PixVolume<'a>) =
                table (typeof<'a>) (x, img)



[<AbstractClass; Sealed; Extension>]
type CommandStreamExtensions private() =

    [<Extension>]
    static member Copy(x : CommandStream, src : 'a[], srcIndex : int, dst : BufferRange, count : int) =
        x.Copy(Memory<'a>(src, srcIndex, count), dst)
        
    [<Extension>]
    static member Copy(x : CommandStream, src : 'a[], dst : BufferRange, count : int) =
        x.Copy(Memory<'a>(src, 0, count), dst)

    [<Extension>]
    static member Copy(x : CommandStream, src : 'a[], dst : BufferRange) =
        x.Copy(Memory<'a>(src, 0, src.Length), dst)
        

    [<Extension>]
    static member Copy(x : CommandStream, src : BufferRange, dst : 'a[], dstIndex : int, count : int) =
        x.Copy(src, Memory(dst, dstIndex, count))
        
    [<Extension>]
    static member Copy(x : CommandStream, src : BufferRange, dst : 'a[], count : int) =
        x.Copy(src, Memory(dst, 0, count))


    [<Extension>]
    static member Copy(x : CommandStream, src : BufferRange, dst : 'a[]) =
        x.Copy(src, Memory(dst, 0, dst.Length))

        
    [<Extension>]
    static member Copy(x : CommandStream, src : Vector<'T>, dst : ImageSubresourceRegion) =
        let info = src.Info
        let tensor =
            Tensor4<'T>(
                src.Data,
                Tensor4Info(
                    info.Origin,
                    V4l(info.Size, 1L, 1L, 1L),
                    V4l(info.Delta, 1L, 1L, 1L)
                )
            )
        x.Copy(tensor, dst)

    [<Extension>]
    static member Copy(x : CommandStream, src : Matrix<'T>, dst : ImageSubresourceRegion) =
        let info = src.Info
        let tensor =
            Tensor4<'T>(
                src.Data,
                Tensor4Info(
                    info.Origin,
                    V4l(info.Size, 1L, 1L),
                    V4l(info.Delta, 1L, 1L)
                )
            )
        x.Copy(tensor, dst)

    [<Extension>]
    static member Copy(x : CommandStream, src : Volume<'T>, dst : ImageSubresourceRegion) =
        let info = src.Info
        let tensor =
            Tensor4<'T>(
                src.Data,
                Tensor4Info(
                    info.Origin,
                    V4l(info.Size, 1L),
                    V4l(info.Delta, 1L)
                )
            )
        x.Copy(tensor, dst)
        
    [<Extension>]
    static member Copy(x : CommandStream, src : PixImage<'T>, dst : ImageSubresourceRegion) =
        x.Copy(src.Volume, dst)
        
    [<Extension>]
    static member Copy(x : CommandStream, src : PixVolume<'T>, dst : ImageSubresourceRegion) =
        x.Copy(src.Tensor4, dst)
 
        
    [<Extension>]
    static member Copy(x : CommandStream, src : PixImage, dst : ImageSubresourceRegion) =
        src.Visit {
            new PixImageVisitor<obj>() with
                member __.Visit<'T when 'T : unmanaged> (img : PixImage<'T>, def : 'T) =
                    x.Copy(img.Volume, dst)
                    null
        } |> ignore
        
    [<Extension>]
    static member Copy(x : CommandStream, src : PixVolume, dst : ImageSubresourceRegion) =
        src.Visit {
            new PixVolumeVisitor<obj>() with
                member __.Visit<'T when 'T : unmanaged> (img : PixVolume<'T>, def : 'T) =
                    x.Copy(img.Tensor4, dst)
                    null
        } |> ignore


    [<Extension>]
    static member Copy(x : CommandStream, src : ImageSubresourceRegion, dst : Vector<'T>) =
        let info = dst.Info
        let tensor =
            Tensor4<'T>(
                dst.Data,
                Tensor4Info(
                    info.Origin,
                    V4l(info.Size, 1L, 1L, 1L),
                    V4l(info.Delta, 1L, 1L, 1L)
                )
            )
        x.Copy(src, tensor)

    [<Extension>]
    static member Copy(x : CommandStream, src : ImageSubresourceRegion, dst : Matrix<'T>) =
        let info = dst.Info
        let tensor =
            Tensor4<'T>(
                dst.Data,
                Tensor4Info(
                    info.Origin,
                    V4l(info.Size, 1L, 1L),
                    V4l(info.Delta, 1L, 1L)
                )
            )
        x.Copy(src, tensor)
        
    [<Extension>]
    static member Copy(x : CommandStream, src : ImageSubresourceRegion, dst : Volume<'T>) =
        let info = dst.Info
        let tensor =
            Tensor4<'T>(
                dst.Data,
                Tensor4Info(
                    info.Origin,
                    V4l(info.Size, 1L),
                    V4l(info.Delta, 1L)
                )
            )
        x.Copy(src, tensor)

    [<Extension>]
    static member Copy(x : CommandStream, src : ImageSubresourceRegion, dst : PixImage<'T>) =
        x.Copy(src, dst.Volume)
        
    [<Extension>]
    static member Copy(x : CommandStream, src : ImageSubresourceRegion, dst : PixVolume<'T>) =
        x.Copy(src, dst.Volume)
        
    [<Extension>]
    static member Copy(x : CommandStream, src : ImageSubresourceRegion, dst : PixImage) =
        dst.Visit {
            new PixImageVisitor<obj>() with
                member __.Visit<'T when 'T : unmanaged> (img : PixImage<'T>, def : 'T) =
                    x.Copy(src, img.Volume)
                    null
        } |> ignore
        
    [<Extension>]
    static member Copy(x : CommandStream, src : ImageSubresourceRegion, dst : PixVolume) =
        dst.Visit {
            new PixVolumeVisitor<obj>() with
                member __.Visit<'T when 'T : unmanaged> (img : PixVolume<'T>, def : 'T) =
                    x.Copy(src, img.Tensor4)
                    null
        } |> ignore
             


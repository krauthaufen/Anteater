namespace Anteater

open System
open System.Runtime.CompilerServices
open Aardvark.Base
open Anteater


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



namespace Anteater

open System
open Anteater
open Aardvark.Base

[<AbstractClass>]
type CommandStream() =
    /// Copy from one BufferRange to another.
    abstract member Copy : src : BufferRange * dst : BufferRange -> unit
    
    /// Copy from a host-pointer to the given Buffer (the host-pointer is assumed to have matching length).
    abstract member Copy : src : nativeint * dst : BufferRange -> unit

    /// Copy from a Buffer to the given host-pointer (the host-pointer is assumed to have matching length).
    abstract member Copy : src : BufferRange * dst : nativeint -> unit


    /// Copy from the given Memory<T> to the given Buffer (the length is set to the minimum of both arguments).
    abstract member Copy<'T when 'T : unmanaged> : src : Memory<'T> * dst : BufferRange -> unit

    /// Copy from the given Buffer to the given Memory<T> (the length is set to the minimum of both arguments).
    abstract member Copy<'T when 'T : unmanaged> :  src : BufferRange * dst : Memory<'T> -> unit
    
    /// Copy from one ImageSubresourceRegion to another.
    abstract member Copy : src : ImageSubresourceRegion * dst : ImageSubresourceRegion -> unit

    
    /// Copy from a host-pointer to the given ImageSubresourceRegion.
    abstract member Copy<'T when 'T : unmanaged> : src : nativeptr<'T> * fmt : Col.Format * dst : ImageSubresourceRegion -> unit

    /// Release all resources associated with CommandStream.
    abstract member Dispose : disposing : bool -> unit

    override x.Finalize() =
        x.Dispose false

    member x.Dispose() =
        GC.SuppressFinalize x
        x.Dispose true

    interface IDisposable with
        member x.Dispose() = x.Dispose()
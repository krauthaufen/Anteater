namespace Anteater

open System
open Aardvark.Base

/// Buffer usage flags telling the backend how buffers can/will be used.
[<Flags>]
type BufferUsage =
    /// The Buffer will not be used at all.
    | None          = 0x000
    /// The Buffer will be used as VertexBuffer.
    | Vertex        = 0x001
    /// The Buffer will be used as IndexBuffer.
    | Index         = 0x002
    /// The Buffer will be used as StorageBuffer.
    | Storage       = 0x004
    /// The Buffer will be used as IndirectBuffer.
    | Indirect      = 0x008
    /// The Buffer will be used as source of Copy operations.
    | CopySrc       = 0x008
    /// The Buffer will be used as destination of Copy operations.
    | CopyDst       = 0x010

/// BufferRange represents a thight range in a buffer ranging from bytes [Offset, Offset + Size).
[<AbstractClass>]
type BufferRange() =
    /// The underlying Buffer.
    abstract member Buffer : Buffer
    /// The byte offset in the Buffer.
    abstract member Offset : int64
    /// The byte size of the Range.
    abstract member Size : int64

    /// Gets a slice of the given BufferRange.
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
        
    /// Gets a slice of the given BufferRange.
    member x.GetSlice(min : option<int>, max : option<int>) =
        x.GetSlice(Option.map int64 min, Option.map int64 max)

    /// Creates a new BufferRange by taking the given number of bytes from the start.
    member x.Take(bytes : int64) =
        if bytes > x.Size then raise <| IndexOutOfRangeException()
        let b = x.Buffer
        let o = x.Offset
        let s = bytes
        { new BufferRange() with
            member __.Buffer = b
            member __.Offset = o
            member __.Size = s
        }
        
    /// Creates a new BufferRange by skipping the given number of bytes from the start.
    member x.Skip(bytes : int64) =
        if bytes > x.Size then raise <| IndexOutOfRangeException()
        let b = x.Buffer
        let o = x.Offset + bytes
        let s = x.Size - bytes
        { new BufferRange() with
            member __.Buffer = b
            member __.Offset = o
            member __.Size = s
        }
        
    /// Creates a new BufferRange for the specified range.
    member x.Sub(offset : int64, size : int64) =
        x.GetSlice(Some offset, Some (offset + size - 1L))
        
    /// Creates a new BufferRange by taking the given number of bytes from the start.
    member x.Take(bytes : int) = x.Take (int64 bytes)

    /// Creates a new BufferRange by skipping the given number of bytes from the start.
    member x.Skip(bytes : int) = x.Skip (int64 bytes)

    /// Creates a new BufferRange for the specified range.
    member x.Sub(offset : int, size : int) =
        x.GetSlice(Some offset, Some (offset + size - 1))

/// Buffer represents a backend-specific buffer.
and Buffer(handle : obj, size : int64, usage : BufferUsage, release : obj -> unit) =
    inherit BufferRange()

    let mutable handle = handle
    let mutable size = size
    let mutable usage = usage

    override x.Offset = 0L
    override x.Buffer = x
    override x.Size = size

    /// The Buffer's backend-specific handle.
    member x.Handle = handle

    /// The BufferUsageFlags given when the Buffer was created.
    member x.Usage = usage

    /// Deletes the Buffer.
    member x.Dispose() =
        if not (isNull handle) then
            release handle
            handle <- null
            size <- 0L
            usage <- BufferUsage.None

    interface IDisposable with
        member x.Dispose() = x.Dispose()

namespace Anteater

open System.Threading.Tasks
open Anteater
open Aardvark.Base

type ImageDescription =
    {
        format  : ImageFormat
        kind    : ImageKind
        array   : bool
        ms      : bool
    }

type ImageFeatures =
    {
        maxSize     : V3i
        maxCount    : int
        render      : bool
        upload      : bool
        download    : bool
        sample      : bool
        samples     : Set<int>
    }

[<AbstractClass>]
type Device() =
    /// A human-readable name for the Device.
    abstract member Name : string

    /// Creates a new Buffer for the given usage with the given size.
    abstract member CreateBuffer : size : int64 * usage : BufferUsage -> Buffer

    /// Creates a new CommandStream for executing commands.
    abstract member CreateCommandStream : unit -> CommandStream

    /// Runs the given CommandStream synchronously.
    abstract member Run : CommandStream -> unit

    /// Runs the given CommandStream as a Task.
    abstract member StartAsTask : CommandStream -> Task

    /// Runs the given CommandStream without waiting for completion.
    abstract member Start : CommandStream -> unit

    /// Releases all resources associated to the device.
    abstract member Dispose : unit -> unit

    interface System.IDisposable with
        member x.Dispose() = x.Dispose()
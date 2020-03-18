namespace Anteater

open System
open Aardvark.Base
open Anteater

[<AbstractClass>]
type CommandStream() =
    abstract member Copy : src : BufferRange * dst : BufferRange -> unit
    abstract member Copy : src : nativeint * dst : BufferRange -> unit
    abstract member Copy : src : BufferRange * dst : nativeint -> unit
    abstract member Copy<'T when 'T : unmanaged> : src : Memory<'T> * dst : BufferRange -> unit
    abstract member Copy<'T when 'T : unmanaged> :  src : BufferRange * dst : Memory<'T> -> unit
    abstract member Dispose : disposing : bool -> unit

    override x.Finalize() =
        x.Dispose false

    member x.Dispose() =
        GC.SuppressFinalize x
        x.Dispose true

    interface IDisposable with
        member x.Dispose() = x.Dispose()


[<AbstractClass>]
type Device() =
    abstract member Name : string

    abstract member CreateBuffer : size : int64 * usage : BufferUsage -> Buffer
    abstract member CreateCommandStream : unit -> CommandStream

    abstract member Run : CommandStream -> unit
    abstract member StartAsTask : CommandStream -> System.Threading.Tasks.Task
    abstract member Start : CommandStream -> unit
    abstract member Dispose : unit -> unit

    interface System.IDisposable with
        member x.Dispose() = x.Dispose()
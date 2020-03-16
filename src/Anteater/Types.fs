namespace Anteater

[<Struct>]
type Buffer(handle : obj, size : int64) =
    member x.Handle = handle
    member x.Size = size

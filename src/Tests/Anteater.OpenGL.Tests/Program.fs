module Program

open Expecto
open Expecto.Impl

[<EntryPoint>]
let main args =
    let cfg = { ExpectoConfig.defaultConfig with ``parallel`` = false }
    runTests cfg Buffers.simple |> ignore
    0
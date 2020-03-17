module Program

open Expecto
open Expecto.Impl

[<EntryPoint>]
let main args =
    runTests ExpectoConfig.defaultConfig Buffers.simple |> ignore
    0
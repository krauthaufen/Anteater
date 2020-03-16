module Program

open Expecto
open Expecto.Impl

[<EntryPoint>]
let main args =
    runTests ExpectoConfig.defaultConfig ContextCreation.simple |> ignore
    0
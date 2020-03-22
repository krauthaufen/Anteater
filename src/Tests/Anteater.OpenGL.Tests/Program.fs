module Program

open Expecto
open Expecto.Impl

let allTests = 
    [
        ContextCreation.simple
        Buffers.simple
        Images.simple
    ]

[<EntryPoint>]
let main args =
    nvidia <- true
    let cfg = { ExpectoConfig.defaultConfig with ``parallel`` = false; verbosity = Logging.LogLevel.Debug }
    runTestsInAssembly cfg [||] |> ignore
    0
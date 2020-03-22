module Program

open Expecto
open Expecto.Impl

[<EntryPoint>]
let main args =
    let logger = Logging.LiterateConsoleTarget([|"expecto"|], Logging.LogLevel.Info)
    nvidia <- args |> Array.exists (fun v -> v.ToLower().Contains "nvidia")

    let cfg = 
        { ExpectoConfig.defaultConfig with 
            ``parallel`` = false
            verbosity = Logging.LogLevel.Info 
            noSpinner = false
            printer = TestPrinters.defaultPrinter
            colour = Logging.Colour256
        }

    runTestsInAssembly cfg [||]
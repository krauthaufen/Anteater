module Program

open Expecto
open Expecto.Impl

[<EntryPoint>]
let main args =
    forceDedicated <- args |> Array.exists (fun v -> v.ToLower().Contains "nvidia")
    let args = args |> Array.filter (fun v -> not (v.ToLower().Contains "nvidia"))

    let cfg = 
        { ExpectoConfig.defaultConfig with 
            ``parallel`` = false
        }

    runTestsInAssembly cfg args
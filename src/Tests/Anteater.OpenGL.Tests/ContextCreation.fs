module ContextCreation

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System
open Utilities

[<Tests>]
let simple = 
    testList "create device" [
    
        testPropertyWithConfig { cfg with maxTest = 3 } "integrated" (fun (f : OpenGLFeatures) ->
            let d = getDevice { queues = 1; nVidia = false; features = f; debug = false }
            ()
        ) 
        testPropertyWithConfig { cfg with maxTest = 3 } "nvidia" (fun (f : OpenGLFeatures) ->
            let d = getDevice { queues = 1; nVidia = true; features = f; debug = false }
            ()
        ) 
    ]




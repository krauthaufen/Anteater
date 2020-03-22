module ContextCreation

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System
open Utilities

[<Tests>]
let simple = 
    testPropertyWithConfig { cfg with maxTest = 10 } "create device" (fun (f : OpenGLFeatures) ->
        let d = getDevice { queues = 1; nVidia = false; features = f; debug = false }
        ()
    ) 




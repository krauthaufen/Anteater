module ContextCreation

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System
open Utilities

[<Tests>]
let simple = 
    testList "Device" [
    
        testPropertyWithConfig cfg "creation" (fun (f : OpenGLFeatures) ->
            use d = new OpenGLDevice { queues = 1; nVidia = false; features = f }
            ()
        ) 
    ]




module ContextCreation

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System
open Utilities

[<Tests>]
let simple = 
    testCase "create device" (fun () ->
        let d = getDevice { queues = 1; nVidia = false; features = OpenGLFeatures.Default; debug = false }
        ()
    ) 




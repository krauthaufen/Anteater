module ContextCreation

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System
open Utilities

[<Tests>]
let tests = 
    testCase "Device.create" (fun () ->
        let d = getDevice { queues = 1; forceDedicated = false; features = OpenGLFeatures.Default; debug = false }
        ()
    ) 




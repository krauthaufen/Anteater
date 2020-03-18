module ContextCreation

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System

[<Tests>]
let simple = 
    testList "ContextCreation" [

        deviceTest "create" OpenGLFeatures.Default (fun _ ->
            ()
        ) 
    ]




module ContextCreation

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System
do Aardvark.Init()

[<Tests>]
let simple = 
    testList "ContextCreation" [

        deviceTest "create" (fun _ ->
            ()
        ) 
    ]




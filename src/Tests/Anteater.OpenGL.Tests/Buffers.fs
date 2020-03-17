module Buffers

open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto
open System
do Aardvark.Init()

[<Tests>]
let simple = 
    
    testList "Buffers" [

        deviceTest "create" (fun d ->
            use _b = d.CreateBuffer(1L <<< 20, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
            ()
        ) 
    ]

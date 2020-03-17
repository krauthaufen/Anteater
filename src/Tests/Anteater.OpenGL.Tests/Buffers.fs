module Buffers

open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto


[<Tests>]
let simple = 
    
    testList "Buffers" [

        deviceTest "create" (fun d ->
            use _b = d.CreateBuffer(1L <<< 20, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
            ()
        ) 

        
        deviceTest "roundtrip" (fun d ->
            let data : int[] = Array.init 1024 id
            let test : int[] = Array.zeroCreate 1024
            use b = d.CreateBuffer(1L <<< 20, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
            let cmd = CommandStream()
            cmd.Copy(data, b)
            cmd.Copy(b, test)
            d.Run cmd
            Expect.equal test data "wrong"
        ) 

    ]

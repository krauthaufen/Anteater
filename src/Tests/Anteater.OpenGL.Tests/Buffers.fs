module Buffers

open System
open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto
open Utilities

[<Tests>]
let simple = 
    
    testList "Buffers" [
        testPropertyWithConfig cfg "create" (fun (f : OpenGLFeatures) ->
            use d = new OpenGLDevice({ queues = 1; nVidia = false; features = f; debug = true })
            use _b = d.CreateBuffer(1L <<< 20, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
            ()
        )
        
        testPropertyWithConfig cfg "roundtrip" (fun (f : OpenGLFeatures) ->
            use d = new OpenGLDevice({ queues = 1; nVidia = false; features = f; debug = true })
            let data : int[] = Array.init 1024 id
            let test : int[] = Array.zeroCreate 1024
            use b = d.CreateBuffer(4096L, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
            use cmd = d.CreateCommandStream()
            cmd.Copy(Memory data, b)
            cmd.Copy(b, Memory test)
            d.Run cmd
            Expect.equal test data "wrong"
        )
        
        testPropertyWithConfig cfg "roundtrip with copy" (fun (f : OpenGLFeatures) ->
            use d = new OpenGLDevice({ queues = 1; nVidia = false; features = f; debug = true })
            let data : int[] = Array.init 1024 id
            let test : int[] = Array.zeroCreate 1024
            use b = d.CreateBuffer(4096L, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
            use c = d.CreateBuffer(4096L, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
            use cmd = d.CreateCommandStream()
            cmd.Copy(Memory data, b)
            cmd.Copy(b, c)
            cmd.Copy(b, Memory test)
            d.Run cmd
            Expect.equal test data "wrong"
        )
    ]

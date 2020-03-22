module Buffers

open System
open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto
open Utilities

[<Tests>]
let simple = 
    
    testList "buffer roundtrip" [
        
        testPropertyWithConfig  { cfg with maxTest = 30 } "simple" (fun (f : OpenGLFeatures) ->
            let d = getDevice { queues = 1; nVidia = false; features = f; debug = true }
            d.DebugSeverity <- 2
            let data : int[] = Array.init 1024 id
            let test : int[] = Array.zeroCreate 1024
            use b = d.CreateBuffer(4096L, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
            use cmd = d.CreateCommandStream()
            cmd.Copy(Memory data, b)
            cmd.Copy(b, Memory test)
            d.Run cmd
            Expect.equal test data "wrong"
        )
        
        testPropertyWithConfig { cfg with maxTest = 30 } "copy" (fun (f : OpenGLFeatures) ->
            let d = getDevice { queues = 1; nVidia = false; features = f; debug = true }
            d.DebugSeverity <- 2
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

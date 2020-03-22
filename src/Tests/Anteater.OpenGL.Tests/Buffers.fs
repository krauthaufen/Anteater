module Buffers

open System
open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto
open Utilities

[<Tests>]
let roundtrip = 
    testList "Buffer.roundtrip" (
        let mask = { OpenGLFeatures.None with directState = true; bufferStorage = true }
        allFeatures (System.Version(4,1)) mask |> Seq.toList |> List.map (fun features ->
            testCase (string features) (fun () ->
                let d = getDevice { queues = 1; forceDedicated = false; features = features; debug = true }
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
        )
    )

[<Tests>]
let withCopy = 
    testList ("Buffer.copy roundtrip") (
        let mask = { OpenGLFeatures.None with directState = true; bufferStorage = true; copyBuffer = true}
        allFeatures (System.Version(4,1)) mask |> Seq.toList |> List.map (fun features ->
            testCase (string features) (fun () ->
                let d = getDevice { queues = 1; forceDedicated = false; features = features; debug = true }
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
        )
    )

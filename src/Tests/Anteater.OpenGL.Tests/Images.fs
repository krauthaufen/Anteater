module Images

open System
open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto
open Utilities

[<Tests>]
let roundtrip = 
    testList "Image.roundtrip" (
        let mask = { OpenGLFeatures.None with directState = true; textureStorage = true; bufferStorage = true}
        allFeatures (System.Version(4,1)) mask |> Seq.toList |> List.map (fun features ->
            testPropertyWithConfig cfg (string features) (fun (info : TextureScenario) ->
                let device = getDevice { queues = 1; forceDedicated = false; features = features; debug = true }
                device.DebugSeverity <- 2
                use img = device.CreateImage(info.dimension, info.format)

                let input = info.CreateTensor()
                let output = info.CreateTensor()

                let range = img.[0,*]

                use cmd = device.CreateCommandStream()
                cmd.Copy(input, range)
                cmd.Copy(range, output)
                device.Run cmd

                let wrong = output.CountWrong input
                Expect.equal wrong 0 "not equal"
            
            )
        )
    )
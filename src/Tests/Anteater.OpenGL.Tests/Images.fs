module Images

open System
open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto
open Utilities

let runImageCopyTest (d : OpenGLDevice) (info : TextureScenario) =
    d.DebugSeverity <- 2
    Log.line "%A: { pix = (%s/%A); format = %A }" info.dimension info.pixFormat.Type.Name info.pixFormat.Format info.format
    use img = d.CreateImage(info.dimension, info.format)

    let input = info.CreateTensor()
    let output = info.CreateTensor()

    let range = img.[0,*]

    use cmd = d.CreateCommandStream()
    cmd.Copy(input, range)
    cmd.Copy(range, output)
    d.Run cmd

    let wrong = output.CountWrong input
    Expect.equal wrong 0 "not equal"

[<Tests>]
let simple = 
    testList "image roundtrip" [
        
        testPropertyWithConfig cfg "dsa" (fun (info : TextureScenario) ->
            let device = 
                getDevice { 
                    queues = 1
                    nVidia = true
                    features = { OpenGLFeatures.Default with directState = true }
                    debug = true 
                }
            runImageCopyTest device info
        )
        testPropertyWithConfig cfg "nodsa" (fun (info : TextureScenario) ->
            let device = 
                getDevice { 
                    queues = 1
                    nVidia = true
                    features = { OpenGLFeatures.Default with directState = false }
                    debug = true 
                }
            runImageCopyTest device info
        )
        testPropertyWithConfig cfg "notexstorage" (fun (info : TextureScenario) ->
            let device = 
                getDevice { 
                    queues = 1
                    nVidia = true
                    features = { OpenGLFeatures.Default with directState = false; textureStorage = false }
                    debug = true 
                }
            runImageCopyTest device info
        )
    ]
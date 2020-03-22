module Images

open System
open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto
open Utilities

let runImageCopyTest (d : OpenGLDevice) (info : TextureScenario) =
    d.DebugSeverity <- 2
    //let  "%A: { pix = (%s/%A); format = %A }" info.dimension info.pixFormat.Type.Name info.pixFormat.Format info.format
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
    testList "image roundtrip" (
        let mask = { OpenGLFeatures.None with directState = true; textureStorage = true; bufferStorage = true}
        allFeatures (System.Version(4,1)) mask |> Seq.toList |> List.map (fun features ->
            testPropertyWithConfig cfg (string features) (fun (info : TextureScenario) ->
                let device = 
                    getDevice { 
                        queues = 1
                        nVidia = true
                        features = features
                        debug = true 
                    }

                match device.TryGetFormatFeatures(info.dimension, info.format) with
                | Some features when features.upload && features.download ->
                    runImageCopyTest device info
                | _ ->
                    () //Log.line "%A: { pix = (%s/%A); format = %A }" info.dimension info.pixFormat.Type.Name info.pixFormat.Format info.format
            
            )
        )

        //if OpenGLDevice.PlatformInfo.features.textureStorage then
        //    testPropertyWithConfig cfg "tstorage: on" (fun (info : TextureScenario) ->
        //        let device = 
        //            getDevice { 
        //                queues = 1
        //                nVidia = true
        //                features = { OpenGLFeatures.Default with directState = false; textureStorage = true }
        //                debug = true 
        //            }
        //        match device.TryGetFormatFeatures(info.dimension, info.format) with
        //        | Some features when features.upload && features.download ->
        //            runImageCopyTest device info
        //        | _ ->
        //            () //Log.warn "%A: { pix = (%s/%A); format = %A }" info.dimension info.pixFormat.Type.Name info.pixFormat.Format info.format
        //    )
            
        //if OpenGLDevice.PlatformInfo.features.bufferStorage then
        //    testPropertyWithConfig cfg "tstorage: on" (fun (info : TextureScenario) ->
        //        let device = 
        //            getDevice { 
        //                queues = 1
        //                nVidia = true
        //                features = { OpenGLFeatures.Default with directState = false; textureStorage = true }
        //                debug = true 
        //            }
        //        match device.TryGetFormatFeatures(info.dimension, info.format) with
        //        | Some features when features.upload && features.download ->
        //            runImageCopyTest device info
        //        | _ ->
        //            () //Log.warn "%A: { pix = (%s/%A); format = %A }" info.dimension info.pixFormat.Type.Name info.pixFormat.Format info.format
        //    )
        //testPropertyWithConfig cfg "notexstorage" (fun (info : TextureScenario) ->
        //    let device = 
        //        getDevice { 
        //            queues = 1
        //            nVidia = true
        //            features = { OpenGLFeatures.Default with directState = false; textureStorage = false }
        //            debug = true 
        //        }

        //    match device.TryGetFormatFeatures(info.dimension, info.format) with
        //    | Some features when features.upload && features.download ->
        //        runImageCopyTest device info
        //    | _ -> 
        //        () //Log.warn "%A: { pix = (%s/%A); format = %A }" info.dimension info.pixFormat.Type.Name info.pixFormat.Format info.format
        //)
    )
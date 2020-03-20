open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Anteater
open Anteater.OpenGL
open Silk.NET.OpenGL
open System.Runtime.InteropServices
open Microsoft.FSharp.Reflection

let allFeatures(version : Version) =
    let fields = FSharpType.GetRecordFields(typeof<OpenGLFeatures>, true) |> Array.rev

    let rec all (i : int) (f : Reflection.PropertyInfo[]) =
        if i >= f.Length then
            [[]]
        elif f.[i].PropertyType = typeof<bool> then
            let rest = all (i+1) f
            rest |> List.collect (fun r ->
                [
                    (false :> obj) :: r
                    (true :> obj) :: r
                ]
            )
        elif f.[i].PropertyType = typeof<Version> then
            all (i+1) f |> List.map (fun r -> (version :> obj) :: r)
        else 
            failwith "non boolean field"
            

    all 0 fields |> Seq.map (fun f ->
        FSharpValue.MakeRecord(typeof<OpenGLFeatures>, Array.rev (Seq.toArray (Seq.cast<obj> f)), true)
        |> unbox<OpenGLFeatures>
    )


[<EntryPoint;STAThread>]
let main argv = 
    Log.start "Buffer copy and up/download"

    let features = 
        { OpenGLFeatures.Default with textureStorage = false; directState = false }


    for f in allFeatures (Version(4,1)) do
        use device = 
            new OpenGLDevice { 
                nVidia = true
                queues = 2
                features = f
                debug = true
            }

        device.DebugSeverity <- 2

        Log.start "%A" device.Features

        let allTargets =
            [|
                TextureTarget.Texture1D
                TextureTarget.Texture1DArray
                
                TextureTarget.Texture2D
                TextureTarget.Texture2DArray
                TextureTarget.Texture2DMultisample
                TextureTarget.Texture2DMultisampleArray

                TextureTarget.TextureCubeMap
                TextureTarget.TextureCubeMapArray

                TextureTarget.Texture3D
            |]

        let findTargets (img : Image) =
            device.Run (fun gl ->
                let _err = gl.GetError()
                allTargets |> Array.filter (fun t ->
                    gl.BindTexture(t, unbox<uint32> img.Handle)
                    let err = gl.GetError()
                    gl.BindTexture(t, 0u)
                    err = GLEnum.None
                )
            )

        let textures =
            [|
                TextureTarget.Texture1D, device.CreateImage(ImageDimension.Image1d 128, ImageFormat.Rgba8UNorm, 2)
                TextureTarget.Texture1DArray, device.CreateImage(ImageDimension.Image1d 1024, ImageFormat.Rgba8UNorm, 11, 5)
                TextureTarget.Texture2D, device.CreateImage(ImageDimension.Image2d(V2i(1024, 1024)), ImageFormat.Rgba8UNorm, 11)
                TextureTarget.Texture2DArray, device.CreateImage(ImageDimension.Image2d(V2i(1024, 1024)), ImageFormat.Rgba8UNorm, 11, 5)
                TextureTarget.Texture2DMultisample, device.CreateImage(ImageDimension.Image2d(V2i(1024, 1024)), ImageFormat.Rgba8UNorm, 11, samples = 4)
                TextureTarget.Texture2DMultisampleArray, device.CreateImage(ImageDimension.Image2d(V2i(1024, 1024)), ImageFormat.Rgba8UNorm, 11, 5, samples = 4)
                TextureTarget.TextureCubeMap, device.CreateImage(ImageDimension.ImageCube 1024, ImageFormat.Rgba8UNorm, 11)
                TextureTarget.TextureCubeMapArray, device.CreateImage(ImageDimension.ImageCube 1024, ImageFormat.Rgba8UNorm, 11, 2)
                TextureTarget.Texture3D, device.CreateImage(ImageDimension.Image3d(V3i(128,128,128)), ImageFormat.Rgba8UNorm, 2)
            |]
            
        device.DebugReport <- false
        for (expected, img) in textures do
            let real = findTargets img
            if not (Array.contains expected real) then
                Log.warn "%A broken (was %A)" expected real
                
            img.Dispose()
        device.DebugReport <- true

        use buffer = device.CreateBuffer(4096L, BufferUsage.CopySrc ||| BufferUsage.CopyDst)
        use buffer2 = device.CreateBuffer(4096L, BufferUsage.CopySrc ||| BufferUsage.CopyDst)
        let data = Array.init 1024 id
        let test : int[] = Array.zeroCreate 1024

        use s = device.CreateCommandStream()
        s.Copy(Memory data, buffer)
        s.Copy(buffer, buffer2)
        s.Copy(buffer2, Memory test)
        device.Run s
        if data <> test then 
            Log.warn "bad"
        Log.stop()
    Log.stop()
    0

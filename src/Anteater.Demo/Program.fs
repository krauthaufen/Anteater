open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Anteater
open Anteater.OpenGL
open Silk.NET.OpenGL
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.NativeInterop
open System.Runtime.InteropServices

#nowarn "9"

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
        {
            OpenGLFeatures.Default with
                directState = false
        }

    for f in [features] do
        use device = 
            new OpenGLDevice { 
                nVidia = true
                queues = 4
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
            
        let size = 1024n * 1024n * 32n * 4n
        let data = Marshal.AllocHGlobal size |> Microsoft.FSharp.NativeInterop.NativePtr.ofNativeInt<byte>

        let mutable ptr = data
        for i in 0L .. int64 size - 1L do
            NativePtr.write ptr (uint8 i)
            ptr <- NativePtr.add ptr 1

        let size = V2i(128, 128)
        use img = device.CreateImage(ImageDimension.Image2d size, ImageFormat.Rgba8UNorm, 2)
        let pimg = PixImage<byte>(Col.Format.RGBA, img.Size.XY)

        let rand = RandomSystem()
        pimg.GetMatrix<C4b>().SetByCoord (fun c -> rand.UniformC3f().ToC4b()) |> ignore

        let timg = PixImage<byte>(Col.Format.RGBA, img.Size.XY)

        use c = device.CreateCommandStream()
        c.Copy(pimg, img.[ImageAspect.Color, 0, 0])
        c.Copy(img.[ImageAspect.Color, 0, 0], timg)
        device.Run c

        let equal = pimg.Volume.InnerProduct(timg.Volume, (=), true, (&&))
        Log.warn "%A" equal

        for (expected, img) in textures do
            device.DebugReport <- false
            let real = findTargets img
            device.DebugReport <- true
            
            if not (Array.contains expected real) then
                Log.warn "%A broken (was %A)" expected real
            elif img.Samples <= 1 then
                ()
                //use c = device.CreateCommandStream()
                //c.Copy(data, Col.Format.RGBA, img.[ImageAspect.Color, 0, *])
                //c.Copy(img.[ImageAspect.Color, 0, *], data, Col.Format.RGBA)
                //device.Run c




            img.Dispose()

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

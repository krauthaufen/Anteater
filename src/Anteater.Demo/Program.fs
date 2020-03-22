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

    let relevant =
        [
            OpenGLFeatures.Default
            { OpenGLFeatures.Default with directState = false }
            { OpenGLFeatures.Default with directState = false; bufferStorage = false }
            { OpenGLFeatures.Default with directState = false; bufferStorage = false; textureStorage = false }
            { OpenGLFeatures.Default with directState = false; textureStorage = false }
        ]

    for f in relevant do
        use device = 
            new OpenGLDevice { 
                forceDedicated = true
                queues = 4
                features = f
                debug = true
            }

        device.DebugSeverity <- 2
        Log.start "%A" device.Features

        let size = V3i(128, 128, 128)
        use img = device.CreateImage(ImageDimension.Image3d size, ImageFormat.R16UNorm, 2)
        let part = img.[0, 0].[V3i(10,10,10) .. V3i(19,19,19)]

        let rand = RandomSystem()
        let pimg = PixVolume<uint16>(Col.Format.Gray, part.Size)
        let timg = PixVolume<uint16>(Col.Format.Gray, part.Size)
        let b24 = (1 <<< 20) - 1
        pimg.ChannelArray.[0].SetByCoord (fun (c : V3l) -> uint16 (rand.UniformInt(b24))) |> ignore
        //pimg.GetMatrix<C4b>().SetByCoord (fun c -> rand.UniformC3f().ToC4b()) |> ignore

        use c = device.CreateCommandStream()
        c.Copy(pimg, part)
        c.Copy(part, timg)
        device.Run c

        let equal = pimg.Tensor4.InnerProduct(timg.Tensor4, (=), true, (&&))
        Log.line "%A" equal
        
        Log.stop()
    Log.stop()
    0

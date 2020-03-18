open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Anteater
open Anteater.OpenGL
open Silk.NET.OpenGL
open System.Runtime.InteropServices
open Microsoft.FSharp.Reflection

let allFeatures(version : Version) =
    let fields = FSharpType.GetRecordFields(typeof<OpenGLFeatures>, true)

    let rec all (i : int) (f : Reflection.PropertyInfo[]) =
        if i >= f.Length then
            [[]]
        elif f.[i].PropertyType = typeof<bool> then
            let rest = all (i+1) f
            rest |> List.collect (fun r ->
                [
                    (true :> obj) :: r
                    (false :> obj) :: r
                ]
            )
        elif f.[i].PropertyType = typeof<Version> then
            all (i+1) f |> List.map (fun r -> (version :> obj) :: r)
        else 
            failwith "non boolean field"
            

    all 0 fields |> Seq.map (fun f ->
        FSharpValue.MakeRecord(typeof<OpenGLFeatures>, Seq.toArray (Seq.cast<obj> f), true)
        |> unbox<OpenGLFeatures>
    )


[<EntryPoint;STAThread>]
let main argv = 
    Log.start "Buffer copy and up/download"
    for f in allFeatures (Version(4,1)) do
        use device = 
            new OpenGLDevice { 
                nVidia = false
                queues = 1
                features = f
            }

        Log.start "%A" device.Features


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

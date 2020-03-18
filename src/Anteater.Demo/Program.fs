open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Anteater
open Anteater.OpenGL
open Silk.NET.OpenGL
open System.Runtime.InteropServices

[<EntryPoint;STAThread>]
let main argv = 
    use d = new OpenGLDevice { version = Version(4,5); nVidia = false; queues = 1 }



    use buffer = d.CreateBuffer(4096L, BufferUsage.CopySrc ||| BufferUsage.CopyDst)
    use buffer2 = d.CreateBuffer(4096L, BufferUsage.CopySrc ||| BufferUsage.CopyDst)
    let data = Array.init 1024 id
    let test : int[] = Array.zeroCreate 1024
    let gc1 = Memory data
    let gc2 = Memory test


    let s = d.CreateCommandStream()
    s.Copy(gc1, buffer)
    s.Copy(buffer, buffer2)
    s.Copy(buffer2, gc2)
    d.Run s

    //let s2 = d.CreateCommandStream()
    //s2.Copy(gc1, buffer)
    //s2.Copy(buffer, buffer2)
    //s2.Copy(buffer2, gc2)
    //d.Run s2
    
    if data <> test then 
        Log.warn "bad"
    else 
        Log.line "good"
    0

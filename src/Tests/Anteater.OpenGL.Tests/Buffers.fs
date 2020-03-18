module Buffers

open System
open Aardvark.Base
open Anteater
open Anteater.OpenGL
open Expecto


[<Tests>]
let simple = 
    
    testList "Buffers" [
        testList "create" (
            allFeatures (System.Version(4,1)) |> List.map (fun f ->
                deviceTest (string f) f (fun d ->
                    use _b = d.CreateBuffer(1L <<< 20, BufferUsage.CopyDst ||| BufferUsage.CopySrc)
                    ()
                ) 
            )
        )
        
        testList "roundtrip" (
            allFeatures (System.Version(4,1)) |> List.map (fun f ->
                deviceTest (string f) f (fun d ->
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
        
        testList "roundtrip with copy" (
            allFeatures (System.Version(4,1)) |> List.map (fun f ->
                deviceTest (string f) f (fun d ->
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
    ]

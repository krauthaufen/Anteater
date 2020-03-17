open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Anteater.OpenGL
open Silk.NET.OpenGL

[<EntryPoint;STAThread>]
let main argv = 
    use d = new Device { version = Version(4,5); nVidia = false; queues = 1 }

    
    Log.line "vendor:  %s" d.Info.vendor
    Log.line "name:    %s" d.Info.renderer
    Log.line "version: OpenGL %A" d.Info.version
    Log.line "glsl:    %A" d.Info.glsl
    Log.start "extensions"
    for e in d.Info.extensions do
        Log.line "%s" e
    Log.stop()

    0

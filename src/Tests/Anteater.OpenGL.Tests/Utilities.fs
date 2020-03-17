[<AutoOpen>]
module Utilities

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System.Runtime.InteropServices
open System.IO
open Anteater

let path = 
    let arch =
        match RuntimeInformation.ProcessArchitecture with
        | Architecture.X64 -> "AMD64"
        | Architecture.X86 -> "x86"
        | a -> failwithf "bad architecture: %A" a
    let plat =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then Path.Combine("windows", arch, "glfw3.dll")
        elif RuntimeInformation.IsOSPlatform OSPlatform.Linux then Path.Combine("linux", arch, "libglfw.so.3")
        elif RuntimeInformation.IsOSPlatform OSPlatform.OSX then Path.Combine("mac", arch, "libglfw.3.dylib")
        else failwith "bad platform"
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "lib", "Native", plat)


let test = NativeLibrary.TryLoad path 

let inline deviceTest (name : string) (action : Device -> unit) =
    testCase name (fun () ->
        use d = new OpenGLDevice { version = System.Version(4,1); queues = 1; nVidia = false }
        action d
    ) 


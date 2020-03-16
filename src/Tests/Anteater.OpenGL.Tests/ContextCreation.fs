module ContextCreation

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System
open System.Runtime.InteropServices
open System.IO

do
    let platform = 
        if isWindows then "windows"
        elif isLinux then "linux"
        else "mac"

    let arch =
        if RuntimeInformation.ProcessArchitecture = Architecture.X64 then "AMD64"
        else "x86"
    
    let libDir =    
        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "lib", "Native", platform, arch)

    Environment.CurrentDirectory <- libDir

[<Tests>]
let simple = 
    testList "ContextCreation" [

        testCase "create" (fun () ->
            let h = ContextHandle.Create (Version(1,4))
            h.Dispose()
        ) 
    ]




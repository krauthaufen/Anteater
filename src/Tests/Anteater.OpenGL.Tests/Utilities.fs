[<AutoOpen>]
module Utilities

open Aardvark.Base
open Anteater.OpenGL
open Expecto
open System.Runtime.InteropServices
open System
open System.IO
open Anteater
open Microsoft.FSharp.Reflection

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

let inline deviceTest (name : string) (features : OpenGLFeatures) (action : Device -> unit) =
    testCase name (fun () ->
        use d = new OpenGLDevice { features = features; queues = 1; nVidia = false; debug = true }
        action d
    ) 


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
            

    all 0 fields |> List.map (fun f ->
        FSharpValue.MakeRecord(typeof<OpenGLFeatures>, Seq.toArray (Seq.cast<obj> f), true)
        |> unbox<OpenGLFeatures>
    )


open FsCheck
type VersionGenerator =
    static member Version() =
        Gen.elements [
            Version(3,3)
            Version(4,1)
            Version(4,3)
            Version(4,5)
        ]
        |> Arb.fromGen


let cfg = { FsCheckConfig.defaultConfig with maxTest = 30; arbitrary = [ typeof<VersionGenerator> ] }

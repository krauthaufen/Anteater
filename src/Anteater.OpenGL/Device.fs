namespace Anteater.OpenGL

open System
open Aardvark.Base
open Silk.NET.OpenGL
open Aardvark.Base
open Microsoft.FSharp.NativeInterop

type DeviceConfig =
    {
        version : Version
        nVidia : bool
    }


type DeviceInfo =
    {
        vendor      : string
        renderer    : string
        version     : Version
        glsl        : Version

        extensions  : Set<string>
    }

module DeviceInfo =
    open System.Text.RegularExpressions

    // major_number.minor_number major_number.minor_number.release_number

    let versionRx = Regex @"^([0-9]+)[ \t]*\.[ \t]*([0-9]+)([ \t]*\.[ \t]*([0-9]+))?"



    let read (gl : GL) =

        let glsl =
            let glsl = gl.GetString(StringName.ShadingLanguageVersion)
            let m = versionRx.Match glsl
            if m.Success then
                if m.Groups.[3].Success then 
                    Version(
                        m.Groups.[1].Value |> int,
                        m.Groups.[2].Value |> int,
                        m.Groups.[4].Value |> int
                    )
                else
                    Version(
                        m.Groups.[1].Value |> int,
                        m.Groups.[2].Value |> int
                    )
            else
                Version(0, 0)

        let glsl = Version(glsl.Major, glsl.Minor / 10, glsl.Minor % 10)

        {
            vendor = gl.GetString(StringName.Vendor)
            renderer = gl.GetString(StringName.Renderer)
            version = Version(gl.GetInteger(GetPName.MajorVersion), gl.GetInteger(GetPName.MinorVersion))
            glsl = glsl

            extensions = Seq.init (gl.GetInteger GetPName.NumExtensions) (fun i -> gl.GetString(StringName.Extensions, uint32 i)) |> Set.ofSeq
        }


type Device(cfg : DeviceConfig) =
    do if cfg.nVidia then DynamicLinker.tryLoadLibrary ("nvapi64" + libraryExtension) |> ignore
    let ctx = ContextHandle.Create(cfg.version)
    let gl = GL.GetApi()

    let info =
        ctx.MakeCurrent()
        let res = DeviceInfo.read gl
        ctx.ReleaseCurrent()
        res

    member x.Info = info

    member x.Dispose() =
        ctx.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()
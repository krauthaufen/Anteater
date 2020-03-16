namespace Anteater.OpenGL

open System.Runtime.InteropServices

[<AutoOpenAttribute>]
module Utilities =
    let isWindows = RuntimeInformation.IsOSPlatform OSPlatform.Windows
    let isOSX = RuntimeInformation.IsOSPlatform OSPlatform.OSX
    let isLinux = RuntimeInformation.IsOSPlatform OSPlatform.Linux

    let libraryExtension =
        if isWindows then ".dll"
        elif isLinux then ".so"
        elif isOSX then ".dylib"
        else ""
        
    let executableExtension =
        if isWindows then ".exe"
        elif isLinux then ""
        elif isOSX then ""
        else ""


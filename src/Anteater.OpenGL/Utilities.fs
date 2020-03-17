namespace Anteater.OpenGL

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"

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

    module VoidPtr =
        let zero = 
            NativePtr.ofNativeInt<byte> 0n |> NativePtr.toVoidPtr
            
        let isNull (ptr : voidptr) =
            let ptr = NativePtr.ofVoidPtr<byte> ptr |> NativePtr.toNativeInt
            ptr = 0n

        let toNativeInt (ptr : voidptr) =
            NativePtr.ofVoidPtr<byte> ptr |> NativePtr.toNativeInt

        let ofNativeInt (ptr : nativeint) =
            NativePtr.ofNativeInt<byte> ptr |> NativePtr.toVoidPtr


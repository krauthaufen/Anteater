namespace Anteater.OpenGL

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open System.IO

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
            
        let ofNativePtr (ptr : nativeptr<'a>) =
            ptr |> NativePtr.toVoidPtr
            
        let toNativePtr<'a when 'a : unmanaged> (ptr : voidptr) =
            NativePtr.ofVoidPtr<'a> ptr


    type ExecutableStream() =
        inherit Stream()

        let mutable capacity = 16n
        let mutable length = 0L
        let mutable ptr = ExecutableMemory.alloc capacity
        let mutable current = ptr

        member x.Pointer = ptr

        override x.Dispose(disposing : bool) =
            if disposing then System.GC.SuppressFinalize x
            ExecutableMemory.free ptr capacity
            ptr <- 0n
            length <- 0L
            current <- 0n
            capacity <- 0n

        override x.CanRead = true
        override x.CanSeek = true
        override x.CanWrite = true

        override x.Length = length
        override x.Position
            with get() = 
                int64 (current - ptr)
            and set (p : int64) =
                let p = clamp 0L length p
                current <- ptr + nativeint p
                
        override x.Flush() = ()

        override x.Seek(offset : int64, origin : SeekOrigin) =
            let offset =
                match origin with
                | SeekOrigin.Begin -> offset
                | SeekOrigin.Current -> int64 (current - ptr) + offset
                | _ -> length + offset
                
            let offset = clamp 0L length offset
            current <- ptr + nativeint offset
            offset

        override x.Read(buffer : byte[], offset : int, count : int) =
            let p = int64 (current - ptr)
            let e = clamp 0L length (p + int64 count)
            let s = e - p |> int
            Marshal.Copy(current, buffer, offset, s)
            current <- current + nativeint s
            s

        override x.Write(buffer : byte[], offset : int, count : int) =
            let p = int64 (current - ptr)
            let e = p + int64 count
            if nativeint e > capacity then
                let newCap = Fun.NextPowerOfTwo (int64 capacity) |> nativeint
                let m = ExecutableMemory.alloc newCap
                Marshal.Copy(ptr, m, length)
                ExecutableMemory.free ptr capacity
                ptr <- m
                current <- m + nativeint p
                capacity <- newCap
            Marshal.Copy(buffer, offset, current, count)
            length <- length + int64 count
            current <- current + nativeint count

        override x.SetLength(l : int64) =
            let lenCap = Fun.NextPowerOfTwo (int64 capacity) |> nativeint
            let p = int64 (current - ptr) |> clamp 0L length
            if lenCap <> capacity then
                let m = ExecutableMemory.alloc lenCap
                Marshal.Copy(ptr, m, min l length)
                ExecutableMemory.free ptr capacity
                ptr <- m
                capacity <- lenCap

            current <- ptr + nativeint p
            length <- l

            
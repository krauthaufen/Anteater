namespace Anteater.OpenGL

open System.Runtime.CompilerServices
open Silk.NET.OpenGL
open Silk.NET.OpenGL.Extensions.ARB
open Aardvark.Base
open Microsoft.FSharp.NativeInterop
open Anteater

#nowarn "9"

module VoidPtr =
    let zero = NativePtr.toVoidPtr NativePtr.zero<byte>

    let ofNativeInt (v : nativeint) =
        NativePtr.toVoidPtr (NativePtr.ofNativeInt<byte> v)

[<AbstractClass; Sealed; Extension>]
type Buffer private() =
    
    static let mutable bufferStorage = ref None
    static let getBufferStorage(gl: GL) =
            match !bufferStorage with
            | Some s -> s
            | None -> 
                lock bufferStorage (fun () ->
                    match gl.TryGetExtension<ArbBufferStorage>() with
                    | (true, ext) -> 
                        bufferStorage := Some (Some ext)
                        Some ext
                    | _ ->
                        bufferStorage := Some None
                        None
                )

    static let mutable directState = ref None
    static let getDirectState(gl: GL) =
            match !directState with
            | Some s -> s
            | None -> 
                lock directState (fun () ->
                    match gl.TryGetExtension<ArbDirectStateAccess>() with
                    | (true, ext) -> 
                        directState := Some (Some ext)
                        Some ext
                    | _ ->
                        directState := Some None
                        None
                )

    static let toBufferStorageMask (usage : BufferUsage) =
        let mutable res = BufferStorageMask.MapReadBit ||| BufferStorageMask.MapWriteBit
        if usage.HasFlag BufferUsage.CopyDst then res <- res ||| BufferStorageMask.DynamicStorageBit
        uint32 res
        
    static let freeBuffer (this : Device) (handle : obj) =
        this.Start(fun gl ->
            gl.DeleteBuffer (unbox<uint32> handle)
        )

    [<Extension>]
    static member CreateBuffer(this : Device, size : int64, usage : BufferUsage) =
        this.Run (fun gl ->
            let handle = gl.GenBuffer()
            match getDirectState gl with
            | Some ext ->
                ext.NamedBufferStorage(handle, uint32 size, VoidPtr.zero, toBufferStorageMask usage)
            | None -> 
                match getBufferStorage gl with
                | Some ext ->
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, handle)
                    ext.BufferStorage(BufferStorageTarget.ArrayBuffer, uint32 size, VoidPtr.zero, toBufferStorageMask usage)
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, 0u)

                | None -> 
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, handle)
                    gl.BufferData(BufferTargetARB.ArrayBuffer, uint32 size, VoidPtr.zero, BufferUsageARB.StaticDraw)
                    gl.BindBuffer(BufferTargetARB.ArrayBuffer, 0u)

           

            new Anteater.Buffer(handle, size, usage, freeBuffer this)
        )

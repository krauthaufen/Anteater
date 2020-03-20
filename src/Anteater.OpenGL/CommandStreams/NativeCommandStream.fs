namespace Anteater.OpenGL


open System
open System.Runtime.InteropServices
open Aardvark.Base
open Silk.NET.OpenGL
open Anteater
open Aardvark.Base.Runtime

[<CommandStreamScore(5)>]
type internal NativeOpenGLCommandStream(device : OpenGLDevice) =
    inherit OpenGLCommandStream()
    
    static let memcpy = 
        if isWindows then 
            let lib = DynamicLinker.loadLibrary("msvcrt.dll")
            DynamicLinker.tryLoadFunction "memcpy" lib
        else
            let lib = DynamicLinker.loadLibrary("libc")
            DynamicLinker.tryLoadFunction "memcpy" lib

    let memory = new ExecutableStream()
    let assembler = AssemblerStream.ofStream memory
    let mutable retOffset = 0L
    let mutable wrapped : Option<nativeint * ExecutableStream * (unit -> unit)> = None

    let preamble = System.Collections.Generic.List<nativeint * (unit -> (unit -> unit))>()

    let alloc (create : unit -> 'a * (unit -> unit)) =
        let ptr = Marshal.AllocHGlobal sizeof<'a>
        preamble.Add(ptr, fun () ->
            let v, release = create()
            NativeInt.write ptr v
            release
        )
        ptr

    static member IsCompatible =
        RuntimeInformation.ProcessArchitecture = Architecture.X64

    member private x.Copy(src : nativeint, dst : BufferRange, srcIndirect : bool) =
        let size = dst.Size
        if size > 0L then
            memory.Position <- retOffset

            let dstOffset = dst.Offset
            let dh = unbox<uint32> dst.Buffer.Handle
            let mapDirect = device.GetProcAddress "glMapNamedBufferRange"
            let unmapDirect = device.GetProcAddress "glUnmapNamedBuffer"
            let bind = device.GetProcAddress "glBindBuffer"
            let map = device.GetProcAddress "glMapBufferRange"
            let unmap = device.GetProcAddress "glUnmapBuffer"

            if mapDirect <> 0n then
                // map
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (int dh)
                assembler.Call mapDirect
                assembler.Mov(assembler.CalleeSavedRegisters.[0], assembler.ReturnRegister)

                // memcpy
                assembler.BeginCall(3)
                assembler.PushArg(nativeint size)
                if srcIndirect then assembler.PushPtrArg src
                else assembler.PushArg src
                assembler.PushArg src
                assembler.Mov(assembler.ArgumentRegisters.[0], assembler.CalleeSavedRegisters.[0])
                assembler.Call(memcpy.Value.Handle)

                // unmap
                assembler.BeginCall(1)
                assembler.PushArg (int dh)
                assembler.Call(unmapDirect)

            else
                // bind
                assembler.BeginCall(2)
                assembler.PushArg(int dh)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                
                // map
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call map
                assembler.Mov(assembler.CalleeSavedRegisters.[0], assembler.ReturnRegister)
                
                // memcpy
                assembler.BeginCall(3)
                assembler.PushArg(nativeint size)
                if srcIndirect then assembler.PushPtrArg src
                else assembler.PushArg src
                assembler.PushArg 0n
                assembler.Mov(assembler.ArgumentRegisters.[0], assembler.CalleeSavedRegisters.[0])
                assembler.Call(memcpy.Value.Handle)
                
                // unmap
                assembler.BeginCall(1)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call(unmap)


                // unbind
                assembler.BeginCall(2)
                assembler.PushArg 0
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                
            retOffset <- memory.Position
            assembler.Ret()
        
    member private x.Copy(src : BufferRange, dst : nativeint, dstIndirect : bool) =
        let size = src.Size
        if size > 0L then
            memory.Position <- retOffset

            let srcOffset = src.Offset
            let sh = unbox<uint32> src.Buffer.Handle
            let bind = device.GetProcAddress "glBindBuffer"
            let map = device.GetProcAddress "glMapBufferRange"
            let unmap = device.GetProcAddress "glUnmapBuffer"
            let mapDirect = device.GetProcAddress "glMapNamedBufferRange"
            let unmapDirect = device.GetProcAddress "glUnmapNamedBuffer"

            if mapDirect <> 0n then
                // map
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapReadBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int sh)
                assembler.Call mapDirect
                assembler.Mov(assembler.CalleeSavedRegisters.[0], assembler.ReturnRegister)

                // memcpy
                assembler.BeginCall(3)
                assembler.PushArg(nativeint size)
                assembler.PushArg dst
                assembler.Mov(assembler.ArgumentRegisters.[1], assembler.CalleeSavedRegisters.[0])
                if dstIndirect then assembler.PushPtrArg dst
                else assembler.PushArg dst
                assembler.Call(memcpy.Value.Handle)

                // unmap
                assembler.BeginCall(1)
                assembler.PushArg (int sh)
                assembler.Call(unmapDirect)

            else
                // bind
                assembler.BeginCall(2)
                assembler.PushArg(int sh)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                
                // map
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapReadBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call map
                assembler.Mov(assembler.CalleeSavedRegisters.[0], assembler.ReturnRegister)
                
                // memcpy
                assembler.BeginCall(3)
                assembler.PushArg(nativeint size)
                assembler.PushArg dst
                assembler.Mov(assembler.ArgumentRegisters.[1], assembler.CalleeSavedRegisters.[0])
                if dstIndirect then assembler.PushPtrArg dst
                else assembler.PushArg dst
                assembler.Call(memcpy.Value.Handle)
                
                // unmap
                assembler.BeginCall(1)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call(unmap)


                // unbind
                assembler.BeginCall(2)
                assembler.PushArg 0
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                
            retOffset <- memory.Position
            assembler.Ret()


    override x.Dispose(_) =
        memory.Dispose()
        for (ptr, _) in preamble do Marshal.FreeHGlobal ptr
        preamble.Clear()
        retOffset <- 0L
        match wrapped with
        | Some (_, e, _) -> 
            e.Dispose()
            wrapped <- None
        | _ ->
            ()

    override x.Copy(src : BufferRange, dst : BufferRange) =
        let size = min src.Size dst.Size
        let srcOffset = src.Offset
        let dstOffset = dst.Offset
        if size > 0L then
            memory.Position <- retOffset

            let sh = unbox<uint32> src.Buffer.Handle
            let dh = unbox<uint32> dst.Buffer.Handle
            let direct = device.GetProcAddress "glCopyNamedBufferSubData"
            let bind = device.GetProcAddress "glBindBuffer"
            let copy = device.GetProcAddress "glCopyBufferSubData"

            if direct <> 0n then
                assembler.BeginCall(5)
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int dh)
                assembler.PushArg (int sh)
                assembler.Call(direct)

            elif copy <> 0n then
                assembler.BeginCall(2)
                assembler.PushArg(int sh)
                assembler.PushArg(int BufferTargetARB.CopyReadBuffer)
                assembler.Call(bind)
                    
                assembler.BeginCall(2)
                assembler.PushArg(int dh)
                assembler.PushArg(int BufferTargetARB.CopyWriteBuffer)
                assembler.Call(bind)

                assembler.BeginCall(5)
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int BufferTargetARB.CopyWriteBuffer)
                assembler.PushArg (int BufferTargetARB.CopyReadBuffer)
                assembler.Call(copy)

                assembler.BeginCall(2)
                assembler.PushArg(0)
                assembler.PushArg(int BufferTargetARB.CopyReadBuffer)
                assembler.Call(bind)
                    
                assembler.BeginCall(2)
                assembler.PushArg(0)
                assembler.PushArg(int BufferTargetARB.CopyWriteBuffer)
                assembler.Call(bind)
            else    
                // mapped CPU copy

                let map = device.GetProcAddress "glMapBufferRange"
                let unmap = device.GetProcAddress "glUnmapBuffer"

                let srcPtr = assembler.CalleeSavedRegisters.[0]
                let dstPtr = assembler.CalleeSavedRegisters.[1]

                // srcPtr = map(src)
                assembler.BeginCall(2)
                assembler.PushArg(int sh)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)
                    
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapReadBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint srcOffset)
                assembler.PushArg (int BufferTargetARB.ArrayBuffer)
                assembler.Call map
                assembler.Mov(srcPtr, assembler.ReturnRegister)

                
                // dstPtr = map(dst)
                assembler.BeginCall(2)
                assembler.PushArg(int dh)
                assembler.PushArg(int BufferTargetARB.ElementArrayBuffer)
                assembler.Call(bind)
                
                assembler.BeginCall(4)
                assembler.PushArg (int (MapBufferAccessMask.MapWriteBit ||| MapBufferAccessMask.MapInvalidateRangeBit))
                assembler.PushArg (nativeint size)
                assembler.PushArg (nativeint dstOffset)
                assembler.PushArg (int BufferTargetARB.ElementArrayBuffer)
                assembler.Call map
                assembler.Mov(dstPtr, assembler.ReturnRegister)


                // copy
                assembler.BeginCall(3)
                assembler.PushArg (nativeint size)
                assembler.PushArg 0n
                assembler.PushArg 0n
                assembler.Mov(assembler.ArgumentRegisters.[0], dstPtr)
                assembler.Mov(assembler.ArgumentRegisters.[1], srcPtr)
                assembler.Call(memcpy.Value.Handle)

                // unmap
                assembler.BeginCall(1)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call unmap

                assembler.BeginCall(1)
                assembler.PushArg(int BufferTargetARB.ElementArrayBuffer)
                assembler.Call unmap

                
                assembler.BeginCall(2)
                assembler.PushArg(0)
                assembler.PushArg(int BufferTargetARB.ArrayBuffer)
                assembler.Call(bind)

                assembler.BeginCall(2)
                assembler.PushArg(0)
                assembler.PushArg(int BufferTargetARB.ElementArrayBuffer)
                assembler.Call(bind)

            retOffset <- memory.Position
            assembler.Ret()

    override x.Copy(src : nativeint, dst : BufferRange) =
        x.Copy(src, dst, false)

    override x.Copy(src : BufferRange, dst : nativeint) =
        x.Copy(src, dst, false)

    override x.Copy<'T when 'T : unmanaged>(src : Memory<'T>, dst : Anteater.BufferRange) =
        let handle = 
            alloc (fun () ->
                let p = src.Pin()
                VoidPtr.toNativeInt p.Pointer, p.Dispose
            )
        x.Copy(handle, dst, true)
        
    override x.Copy<'T when 'T : unmanaged>(src : Anteater.BufferRange, dst : Memory<'T>) =
        let handle = 
            alloc (fun () ->
                let p = dst.Pin()
                VoidPtr.toNativeInt p.Pointer, p.Dispose
            )
        x.Copy(src, handle, true)


    override x.Copy(src : ImageSubresourceRegion, dst : ImageSubresourceRegion) : unit =
        failwith "not implemented"
        
    override x.Run(ctx : ContextHandle, gl : GL) =
        let run = 
            match wrapped with
            | Some (ptr, _, run) when ptr = memory.Pointer -> run
            | _ ->
                match wrapped with
                | Some (_, e, _) -> e.Dispose()
                | None -> ()

                let e = new ExecutableStream()
                let s = AssemblerStream.ofStream e
                for r in assembler.CalleeSavedRegisters do s.Push r
                s.BeginCall(0)
                s.Call(memory.Pointer)
                for r in assembler.CalleeSavedRegisters do s.Pop r
                s.Ret()

                let run : unit -> unit = UnmanagedFunctions.wrap e.Pointer
                wrapped <- Some (memory.Pointer, e, run)
                run

        let release = Array.zeroCreate preamble.Count
        let mutable i = 0
        for (_, p) in preamble do
            release.[i] <- p()
            i <- i + 1

        run()

        for r in release do r()
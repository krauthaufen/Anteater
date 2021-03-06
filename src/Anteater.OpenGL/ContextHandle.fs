﻿namespace Anteater.OpenGL

open System
open Silk.NET.GLFW
open Aardvark.Base
open Microsoft.FSharp.NativeInterop

#nowarn "9"

module Glfw =

    type internal GlfwLoader(glfw : Glfw) =
        inherit Silk.NET.Core.Loader.GLSymbolLoader()

        override x.CoreLoadFunctionPointer(lib : nativeint, name : string) =
            glfw.GetProcAddress(name)


    let api =
        //Ultz.SuperInvoke.LibraryActivator.CreateInstance<Glfw>()
        let a = Glfw.GetApi()
        if not (a.Init()) then failwith "[GLFW] could not initialize"
        do Silk.NET.Core.Platform.SilkManager.Register<Silk.NET.Core.Loader.GLSymbolLoader>(GlfwLoader a)
        a

    //let mutable private lastWindow : nativeptr<WindowHandle> = NativePtr.zero

    let internal createContext (debug : bool) (sharedWith : nativeptr<WindowHandle>) (v : Version) =
        lock api (fun () ->
            api.DefaultWindowHints()
            api.WindowHint(WindowHintBool.Visible, false)
            api.WindowHint(WindowHintBool.OpenGLDebugContext, debug)
            if v.Major > 0 then
                api.WindowHint(WindowHintOpenGlProfile.OpenGlProfile, OpenGlProfile.Core)
                api.WindowHint(WindowHintInt.ContextVersionMajor, v.Major)
                api.WindowHint(WindowHintInt.ContextVersionMinor, v.Minor)
            else
                api.WindowHint(WindowHintOpenGlProfile.OpenGlProfile, OpenGlProfile.Any)
            let win = api.CreateWindow(8,8, "", NativePtr.zero, sharedWith)
            if NativePtr.isNull win then failwithf "[GLFW] could not create context"
            //lastWindow <- win
            win
        )

type ContextHandle private(glfw : Glfw, handle : nativeptr<WindowHandle>, dispose : bool) as this =
    static let dict = System.Collections.Concurrent.ConcurrentDictionary<nativeptr<WindowHandle>, ContextHandle>()
    
    let mutable handle = handle
    do dict.TryAdd(handle, this) |> ignore

    static member Current =
        let h = Glfw.api.GetCurrentContext()
        match dict.TryGetValue h with
        | (true, v) -> Some v
        | _ -> None

    member private x.Window = handle

    member x.IsCurrent =
        handle <> NativePtr.zero && glfw.GetCurrentContext() = handle

    member x.MakeCurrent() =
        if handle = NativePtr.zero then raise <| System.ObjectDisposedException("Context")

        let o = glfw.GetCurrentContext()
        if o <> NativePtr.zero then glfw.MakeContextCurrent(NativePtr.zero)

        glfw.MakeContextCurrent(handle)

    member x.ReleaseCurrent() =
        if handle = NativePtr.zero then raise <| System.ObjectDisposedException("Context")

        let o = glfw.GetCurrentContext()
        if o = handle then glfw.MakeContextCurrent(NativePtr.zero)
        
    member x.Dispose() =
        if dispose && handle <> NativePtr.zero then
            dict.TryRemove handle |> ignore
            glfw.DestroyWindow(handle)
            handle <- NativePtr.zero

    member x.GetProcAddress(name : string) =
        glfw.GetProcAddress(name)

    member x.Pointer = NativePtr.toNativeInt handle

    override x.ToString() =
        sprintf "GL[0x%X]" (NativePtr.toNativeInt handle)

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    static member Create(v : Version, ?debug : bool, ?sharedWith : ContextHandle) =
        let ptr =
            match sharedWith with
            | Some c -> c.Window
            | _ -> NativePtr.zero

        let debug = defaultArg debug false

        new ContextHandle(Glfw.api, Glfw.createContext debug ptr v, true)

    static member FromWindow(windowHandle : nativeptr<WindowHandle>) =
        new ContextHandle(Glfw.api, windowHandle, false)






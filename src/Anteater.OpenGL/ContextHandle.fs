namespace Anteater.OpenGL

open System
open Silk.NET.GLFW
open Aardvark.Base
open Microsoft.FSharp.NativeInterop

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

    let mutable private lastWindow : nativeptr<WindowHandle> = NativePtr.zero

    let internal createContext (v : Version) =
        lock api (fun () ->
            api.DefaultWindowHints()
            api.WindowHint(WindowHintBool.Visible, false)
            if v.Major > 0 then
                api.WindowHint(WindowHintOpenGlProfile.OpenGlProfile, OpenGlProfile.Core)
                api.WindowHint(WindowHintInt.ContextVersionMajor, v.Major)
                api.WindowHint(WindowHintInt.ContextVersionMinor, v.Minor)
            else
                api.WindowHint(WindowHintOpenGlProfile.OpenGlProfile, OpenGlProfile.Any)
            let win = api.CreateWindow(8,8, "", NativePtr.zero, lastWindow)
            if NativePtr.isNull win then failwithf "[GLFW] could not create context"
            lastWindow <- win
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

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    static member Create(v : Version) =
        new ContextHandle(Glfw.api, Glfw.createContext v, true)

    static member FromWindow(windowHandle : nativeptr<WindowHandle>) =
        new ContextHandle(Glfw.api, windowHandle, false)






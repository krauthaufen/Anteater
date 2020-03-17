namespace Anteater.OpenGL

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent
open Aardvark.Base
open Silk.NET.OpenGL
open Aardvark.Base
open Microsoft.FSharp.NativeInterop

type DeviceConfig =
    {
        version : Version
        nVidia : bool
        queues : int
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


    static let deviceThread (ct : CancellationToken) (queue : BlockingCollection<GL -> unit>) (ctx : ContextHandle) (gl : GL) () =
        ctx.MakeCurrent()
        try
            try
                for e in queue.GetConsumingEnumerable(ct) do
                    try e gl
                    with _ -> ()
            with :? OperationCanceledException ->
                ()
        finally 
            ctx.ReleaseCurrent()

    let contexts =
        Array.init (max cfg.queues 1) (fun i ->
            ContextHandle.Create(cfg.version)
        )
        
    let gl = GL.GetApi()

    let info =
        contexts.[0].MakeCurrent()
        let res = DeviceInfo.read gl
        contexts.[0].ReleaseCurrent()
        res

    let cancel = new CancellationTokenSource()
    let queue = new BlockingCollection<GL -> unit>()
    let threads =
        contexts |> Array.mapi (fun id ctx ->
            let thread = 
                Thread(
                    ThreadStart(deviceThread cancel.Token queue ctx gl),
                    IsBackground = true,
                    Name = sprintf "OpenGL[%d]" id,
                    Priority = ThreadPriority.Highest
                )
            thread.Start()
            thread
        )

    member x.Info = info

    member x.Dispose() =
        cancel.Cancel()
        for t in threads do t.Join()
        queue.Dispose()
        cancel.Dispose()

    member x.Start(action : GL -> unit) =
        queue.Add (fun gl ->
            action gl
            gl.Flush()
            gl.Finish()
        )

    member x.StartAsTask(action : GL -> 'a) =
        let tcs = TaskCompletionSource<'a>()
        let action gl =
            try
                let res = action gl
                gl.Flush()
                gl.Finish()
                tcs.SetResult res
            with e ->
                tcs.SetException e
        queue.Add action
        tcs.Task

    member x.Run(action : GL -> 'a) =
        let res : ref<option<Choice<'a, exn>>> = ref None
        let action gl =
            try
                let value = action gl
                gl.Flush()
                gl.Finish()
                lock res (fun () -> 
                    res := Some (Choice1Of2 value)
                    Monitor.PulseAll res
                )
            with e ->
                lock res (fun () ->
                    res := Some (Choice2Of2 e)
                    Monitor.PulseAll res

                )
        queue.Add action
        lock res (fun () ->
            while Option.isNone !res do
                Monitor.Wait res |> ignore
        )
        match res.Value.Value with
        | Choice1Of2 v -> v
        | Choice2Of2 e -> raise e
 
    interface IDisposable with
        member x.Dispose() = x.Dispose()
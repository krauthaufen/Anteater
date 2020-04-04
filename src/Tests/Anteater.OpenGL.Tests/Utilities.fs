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

let mutable forceDedicated = Environment.GetEnvironmentVariable("NVIDIA") |> isNull |> not

[<AutoOpen>]
module DeviceExtensions =
    let private info =
        lazy (
            use d = new OpenGLDevice { queues = 1; features = OpenGLFeatures.Default; debug = false; forceDedicated = forceDedicated }
            d.Info
        )

    type OpenGLDevice with
        static member PlatformInfo = info.Value


    type DeviceInfo with
        
        member x.TryGetFormatFeatures(dim : ImageDimension, format : ImageFormat, ?levels : int, ?slices : int, ?samples : int) =
            let isArray = Option.isSome slices
            let levels = defaultArg levels 1
            let samples = defaultArg samples 1
            let slices = defaultArg slices 1
            let size = dim.GetImageSize(1)

            let key = { format = format; kind = dim.Kind; array = isArray; ms = samples > 1 }
            match Map.tryFind key x.formats with
            | Some info ->
                if size.AllSmallerOrEqual info.maxSize && slices <= info.maxCount && Set.contains samples info.samples then
                    Some info
                else 
                    None
            | _ ->
                None

        member x.CanTestRoundtrip(dim : ImageDimension, format : ImageFormat, ?levels : int, ?slices : int, ?samples : int) =
            match x.TryGetFormatFeatures(dim, format, ?levels = levels, ?slices = slices, ?samples = samples) with
            | Some i -> i.upload && i.download
            | None -> false


let allFeatures (version : Version) (mask : OpenGLFeatures) =
    let fields = FSharpType.GetRecordFields(typeof<OpenGLFeatures>, true) |> Array.rev

    let rec all (i : int) (f : Reflection.PropertyInfo[]) =
        if i >= f.Length then
            [[]]
        elif f.[i].PropertyType = typeof<bool> then
            if f.[i].GetValue(OpenGLDevice.PlatformInfo.features) |> unbox && f.[i].GetValue(mask) |> unbox then
                let rest = all (i+1) f
                rest |> List.collect (fun r ->
                    [
                        (false :> obj) :: r
                        (true :> obj) :: r
                    ]
                )
            else
                let rest = all (i+1) f
                rest |> List.map (fun r ->
                    (false :> obj) :: r
                )
                
        elif f.[i].PropertyType = typeof<Version> then
            all (i+1) f |> List.map (fun r -> (version :> obj) :: r)
        else 
            failwith "non boolean field"
            

    all 0 fields |> Seq.map (fun f ->
        FSharpValue.MakeRecord(typeof<OpenGLFeatures>, Array.rev (Seq.toArray (Seq.cast<obj> f)), true)
        |> unbox<OpenGLFeatures>
    )
    |> Seq.groupBy (fun f -> f.directState)
    |> Seq.collect (fun (dsa, rest) ->
        if dsa then Seq.singleton (Seq.last rest)
        else rest
    )

open FsCheck

type TextureScenario =
    {
        pixFormat : PixFormat
        dimension : ImageDimension
        format    : ImageFormat
        slices    : int
    }

[<AutoOpen>]
module TextureScenario =
    
    type private Random<'T>() =
        static let create =
            if typeof<'T> = typeof<uint8> then (fun (rand : RandomSystem) -> rand.UniformInt(256) |> byte) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<int8> then (fun (rand : RandomSystem) -> rand.UniformInt(256) - 128 |> int8) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<uint16> then (fun (rand : RandomSystem) -> rand.UniformInt(65536) |> uint16) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<int16> then (fun (rand : RandomSystem) -> rand.UniformInt(65536) - 32768 |> int16) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<uint24> then (fun (rand : RandomSystem) -> rand.UniformInt() |> uint24) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<uint32> then (fun (rand : RandomSystem) -> rand.UniformInt() |> uint32) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<int32> then (fun (rand : RandomSystem) -> rand.UniformInt()) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<float32> then (fun (rand : RandomSystem) -> rand.UniformFloat()) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<float> then (fun (rand : RandomSystem) -> rand.UniformDouble()) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<float16> then (fun (rand : RandomSystem) -> float16(Float32 = rand.UniformFloat())) |> unbox<RandomSystem -> 'T>
            elif typeof<'T> = typeof<Depth24Stencil8> then (fun (rand : RandomSystem) -> Depth24Stencil8(rand.UniformUInt())) |> unbox<RandomSystem -> 'T>
            else failwithf "bad type: %A" typeof<'T>

        static member Generate(rand : RandomSystem) = create rand

    type private TypeVisitor<'R> =
        abstract Accept<'T> : unit -> 'R

    let private visit (t : Type) (v : TypeVisitor<'R>) =
        let m = typeof<TypeVisitor<'R>>.GetMethod "Accept"
        let mi = m.MakeGenericMethod [| t |]
        mi.Invoke(v, [||]) |> unbox<'R>

    let private random = RandomSystem()

    type TensorVisitor<'R> =
        abstract member Accept<'T when 'T : unmanaged> : Vector<'T> -> 'R
        abstract member Accept<'T when 'T : unmanaged> : Matrix<'T> -> 'R
        abstract member Accept<'T when 'T : unmanaged> : Volume<'T> -> 'R
        abstract member Accept<'T when 'T : unmanaged> : Tensor4<'T> -> 'R

    type Tensor =
        {
            tensorType : Type
            tensor      : obj
        }

        member x.Visit(v : TensorVisitor<'R>) =
            let ms = 
                typeof<TensorVisitor<'R>>.GetMethods() 
                |> Array.filter (fun m -> m.Name = "Accept" && m.IsGenericMethod)
                |> Array.map (fun m -> m.MakeGenericMethod (x.tensorType.GetGenericArguments()) :> Reflection.MethodBase)
            let m = Type.DefaultBinder.SelectMethod(Reflection.BindingFlags.Default, ms, [| x.tensorType |], null)
            m.Invoke(v, [| x.tensor |]) |> unbox<'R>

        member x.CountWrong (other : Tensor) =
            if x.tensorType = other.tensorType then
                x.Visit { 
                    new TensorVisitor<int> with
                        member __.Accept<'T when 'T : unmanaged>(v : Vector<'T>) : int =
                            let other = unbox<Vector<'T>> other.tensor
                            v.InnerProduct(other, (fun l r -> if Unchecked.equals l r then 0 else 1), 0, (+))
                        member __.Accept<'T when 'T : unmanaged>(v : Matrix<'T>) : int =
                            let other = unbox<Matrix<'T>> other.tensor
                            v.InnerProduct(other, (fun l r -> if Unchecked.equals l r then 0 else 1), 0, (+))
                        member __.Accept<'T when 'T : unmanaged>(v : Volume<'T>) : int =
                            let other = unbox<Volume<'T>> other.tensor
                            v.InnerProduct(other, (fun l r -> if Unchecked.equals l r then 0 else 1), 0, (+))
                        member __.Accept<'T when 'T : unmanaged>(v : Tensor4<'T>) : int =
                            let other = unbox<Tensor4<'T>> other.tensor
                            v.InnerProduct(other, (fun l r -> if Unchecked.equals l r then 0 else 1), 0, (+))
                }

            else
                Int32.MaxValue

    let private createTensor (info : TextureScenario) =
        let channels = info.pixFormat.ChannelCount
        visit info.pixFormat.Type {
            new TypeVisitor<Tensor> with
                member x.Accept<'T>() =
                    match info.dimension with
                    | ImageDimension.Image1d s ->
                        let m = Volume<'T>(V3i(s, channels, info.slices))
                        m.SetByIndex(fun _ -> Random<'T>.Generate random) |> ignore
                        { tensorType = m.GetType(); tensor = m :> obj }
                    | ImageDimension.Image2d s ->
                        let m = Tensor4<'T>(V4i(s, channels, info.slices))
                        m.SetByIndex(fun _ -> Random<'T>.Generate random) |> ignore
                        { tensorType = m.GetType(); tensor = m :> obj }
                    | ImageDimension.ImageCube s ->
                        let m = Tensor4<'T>(V4i(s, s, channels, 6 * info.slices))
                        m.SetByIndex(fun _ -> Random<'T>.Generate random) |> ignore
                        { tensorType = m.GetType(); tensor = m :> obj }
                    | ImageDimension.Image3d s ->
                        let m = Tensor4<'T>(V4i(s, channels))
                        m.SetByIndex(fun _ -> Random<'T>.Generate random) |> ignore
                        { tensorType = m.GetType(); tensor = m :> obj }
                    
        }

    type TextureScenario with
        member x.CreateTensor() =
            createTensor x

    type CommandStream with
        member x.Copy(src : Tensor, dst : ImageSubresourceRegion) =
            src.Visit { 
                new TensorVisitor<obj> with
                    member __.Accept<'T when 'T : unmanaged>(v : Vector<'T>) : obj = x.Copy(v, dst); null
                    member __.Accept<'T when 'T : unmanaged>(v : Matrix<'T>) : obj  = x.Copy(v, dst); null
                    member __.Accept<'T when 'T : unmanaged>(v : Volume<'T>) : obj  = x.Copy(v, dst); null
                    member __.Accept<'T when 'T : unmanaged>(v : Tensor4<'T>) : obj = x.Copy(v, dst); null
            } |> ignore
            
        member x.Copy(src : ImageSubresourceRegion, dst : Tensor) =
            dst.Visit { 
                new TensorVisitor<obj> with
                    member __.Accept<'T when 'T : unmanaged>(v : Vector<'T>) : obj = x.Copy(src, v); null
                    member __.Accept<'T when 'T : unmanaged>(v : Matrix<'T>) : obj  = x.Copy(src, v); null
                    member __.Accept<'T when 'T : unmanaged>(v : Volume<'T>) : obj  = x.Copy(src, v); null
                    member __.Accept<'T when 'T : unmanaged>(v : Tensor4<'T>) : obj = x.Copy(src, v); null
            } |> ignore



type VersionGenerator =

    static member OpenGLFeatures() =
        gen {
            let features = OpenGLDevice.PlatformInfo.features

            let allFields = 
                FSharpType.GetRecordFields (typeof<OpenGLFeatures>)

            let values : obj[] = Array.zeroCreate allFields.Length

            for i in 0 .. allFields.Length - 1 do
                let f = allFields.[i]
                if f.PropertyType = typeof<bool> then
                    let v = f.GetValue(features) |> unbox<bool>
                    if v then
                        let! r = Arb.generate<bool>
                        values.[i] <- r :> obj
                    else
                        values.[i] <- false :> obj
                else
                    values.[i] <- f.GetValue features


            let res = FSharpValue.MakeRecord(typeof<OpenGLFeatures>, values) |> unbox<OpenGLFeatures>

            return res

        }
        |> Arb.fromGen

    static member Version() =
        Gen.elements [
            Version(3,3)
            Version(4,1)
            Version(4,3)
            Version(4,5)
        ]
        |> Arb.fromGen

    static member TextureScenario() =
        gen {
            let! format = 
                Arb.generate<ImageFormat> 
                |> Gen.filter (fun f -> not (ImageFormat.hasDepth f))
                

            let set = ImageFormat.channelTypes format
            let! typ = Gen.elements set
            let channels = ImageFormat.channels format
            let pixFormat =
                match channels with
                | 1 -> PixFormat(typ, Col.Format.Gray)
                | 2 -> PixFormat(typ, Col.Format.NormalUV)
                | 3 -> PixFormat(typ, Col.Format.RGB)
                | _ -> PixFormat(typ, Col.Format.RGBA)

            let! dim = Gen.choose (1,2)
            let mutable imageDim = ImageDimension.Image1d 0
            let align (s : int) =
                let v = s &&& ~~~3
                if v = 0 then 4
                else v
            match dim with
            | 1 ->
                let! s = Gen.choose (1, 512)
                imageDim <- ImageDimension.Image1d (align s)
            | 2 ->
                let! sx = Gen.choose (1, 512)
                let! sy = Gen.choose (1, 512)
                imageDim <- ImageDimension.Image2d (V2i(align sx, align sy))
            | 3 ->
                let! s = Gen.choose (1, 512)
                imageDim <- ImageDimension.ImageCube (align s)
            | _ ->
                let! sx = Gen.choose (1, 128)
                let! sy = Gen.choose (1, 128)
                let! sz = Gen.choose (1, 128)
                imageDim <- ImageDimension.Image3d(V3i(align sx,align sy,align sz))

            //let! slices = 
            //    if dim > 3 then Gen.elements [1]
            //    else Gen.choose (1, 3)

            return {
                pixFormat = pixFormat
                dimension = imageDim
                format    = format
                slices    = 1
            }

        } 
        |> Gen.filter (fun info -> OpenGLDevice.PlatformInfo.CanTestRoundtrip(info.dimension, info.format))
        |> Arb.fromGen


let cfg = { FsCheckConfig.defaultConfig with maxTest = 100; arbitrary = [ typeof<VersionGenerator> ] }


let private devices = System.Collections.Concurrent.ConcurrentDictionary<OpenGLDeviceConfig, OpenGLDevice>()

let getDevice (features : OpenGLDeviceConfig) =
    devices.GetOrAdd(features, fun features ->
        new OpenGLDevice { features with forceDedicated = forceDedicated }
    )


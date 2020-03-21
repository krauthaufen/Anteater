namespace Anteater

open System
open System.Runtime.InteropServices

#nowarn "9"
#nowarn "77"

[<StructLayout(LayoutKind.Explicit, Size = 3)>]
[<StructuredFormatDisplay("{AsString}"); CustomComparison; CustomEquality>]
type UInt24 =
    struct
        [<FieldOffset(0)>]
        val mutable public B0 : byte
        [<FieldOffset(1)>]
        val mutable public B1 : byte
        [<FieldOffset(2)>]
        val mutable public B2 : byte
        
        member private x.AsString = sprintf "%A" (uint32 x)
        override x.ToString() = uint32(x).ToString()
        member x.ToString(format : string) = uint32(x).ToString(format)
        member x.ToString(provider : IFormatProvider) = uint32(x).ToString(provider)
        member x.ToString(format : string, provider : IFormatProvider) = uint32(x).ToString(format, provider)

        static member MaxValue = uint24 16777215u
        static member MinValue = uint24 0uy

        static member op_Explicit(x : uint24) : uint8 =
            x.B0 

        static member op_Explicit(x : uint24) : uint16 =
            uint16 x.B0 |||
            (uint16 x.B1 <<< 8)

        static member op_Explicit(x : uint24) : uint32 =
            uint32 x.B0 |||
            (uint32 x.B1 <<< 8) |||
            (uint32 x.B2 <<< 16)
            
        static member op_Explicit(x : uint24) : uint64 =
            uint64 x.B0 |||
            (uint64 x.B1 <<< 8) |||
            (uint64 x.B2 <<< 16)
            
        static member op_Explicit(x : uint24) : int8 =
            int8 x.B0 

        static member op_Explicit(x : uint24) : int16 =
            int16 x.B0 |||
            (int16 x.B1 <<< 8)

        static member op_Explicit(x : uint24) : int32 =
            int32 x.B0 |||
            (int32 x.B1 <<< 8) |||
            (int32 x.B2 <<< 16)
            
        static member op_Explicit(x : uint24) : int64 =
            int64 x.B0 |||
            (int64 x.B1 <<< 8) |||
            (int64 x.B2 <<< 16)
            
        static member op_Explicit(x : uint24) : char =
            char (uint16 x.B0 ||| (uint16 x.B1 <<< 8))

        static member op_Explicit(x : uint24) : float32 =
            float32 (
                uint32 x.B0 |||
                (uint32 x.B1 <<< 8) |||
                (uint32 x.B2 <<< 16)
            )

        static member op_Explicit(x : uint24) : float =
            float (
                uint32 x.B0 |||
                (uint32 x.B1 <<< 8) |||
                (uint32 x.B2 <<< 16)
            )

        static member op_Explicit(x : uint24) : decimal =
            decimal (
                uint32 x.B0 |||
                (uint32 x.B1 <<< 8) |||
                (uint32 x.B2 <<< 16)
            )
        static member op_Explicit(x : uint24) : nativeint =
            nativeint (
                uint32 x.B0 |||
                (uint32 x.B1 <<< 8) |||
                (uint32 x.B2 <<< 16)
            )
        static member op_Explicit(x : uint24) : unativeint =
            unativeint (
                uint32 x.B0 |||
                (uint32 x.B1 <<< 8) |||
                (uint32 x.B2 <<< 16)
            )
        
        
        static member op_Explicit(v : float32) : uint24 = uint24(uint32 v)
        static member op_Explicit(v : float) : uint24 = uint24(uint32 v)
        static member op_Explicit(v : decimal) : uint24 = uint24(uint32 v)
        static member op_Explicit(v : char) : uint24 = uint24(uint16 v)
        static member op_Explicit(v : nativeint) : uint24 = uint24(uint32 v)
        static member op_Explicit(v : unativeint) : uint24 = uint24(uint32 v)
        static member op_Explicit(v : uint8) : uint24 = uint24(v)
        static member op_Explicit(v : uint16) : uint24 = uint24(v)
        static member op_Explicit(v : uint32) : uint24 = uint24(v)
        static member op_Explicit(v : uint64) : uint24 = uint24(v)
        static member op_Explicit(v : int8) : uint24 = uint24(v)
        static member op_Explicit(v : int16) : uint24 = uint24(v)
        static member op_Explicit(v : int32) : uint24 = uint24(v)
        static member op_Explicit(v : int64) : uint24 = uint24(v)

        static member Zero = uint24(0uy)
        static member One = uint24(1uy)
        static member inline (+) (l : uint24, r : uint24) : uint24 = uint32 l + uint32 r |> uint24
        static member inline (-) (l : uint24, r : uint24) : uint24 = uint32 l - uint32 r |> uint24
        static member inline (*) (l : uint24, r : uint24) : uint24 = uint32 l * uint32 r |> uint24
        static member inline (/) (l : uint24, r : uint24) : uint24 = uint32 l / uint32 r |> uint24
        
        static member inline (<<<) (l : uint24, r : int) : uint24 = (uint32 l <<< r) |> uint24
        static member inline (>>>) (l : uint24, r : int) : uint24 = (uint32 l >>> r) |> uint24
        
        static member inline (~~~) (l : uint24) : uint24 = ~~~(uint32 l) |> uint24
        static member inline (|||) (l : uint24, r : uint24) : uint24 = (uint32 l ||| uint32 r) |> uint24
        static member inline (&&&) (l : uint24, r : uint24) : uint24 = (uint32 l &&& uint32 r) |> uint24
        static member inline (^^^) (l : uint24, r : uint24) : uint24 = (uint32 l ^^^ uint32 r) |> uint24
        
        static member op_LessThan (l : uint24, r : uint24) = uint32 l < uint32 r
        static member op_LessThanOrEqual (l : uint24, r : uint24) = uint32 l <= uint32 r
        static member op_GreaterThan (l : uint24, r : uint24) = uint32 l > uint32 r
        static member op_GreaterThanOrEqual (l : uint24, r : uint24) = uint32 l >= uint32 r
        static member op_Equality (l : uint24, r : uint24) = uint32 l = uint32 r
        static member op_Inequality (l : uint24, r : uint24) = uint32 l <> uint32 r



        member x.CompareTo(o : uint24) =
            compare (uint32 x) (uint32 o)

        interface IComparable with
            member x.CompareTo(o : obj) =
                match o with
                | :? uint24 as o -> compare (uint32 x) (uint32 o)
                | _ -> failwith "uncomparable"
                
        interface IComparable<uint24> with
            member x.CompareTo(o : uint24) =
                compare (uint32 x) (uint32 o)
                
        interface IEquatable<uint24> with
            member x.Equals(o : uint24) =
                (uint32 x) = (uint32 o)

        override x.GetHashCode() = 
            int x

        override x.Equals(o : obj) =
            match o with
            | :? uint24 as o -> uint32 x = uint32 o
            | _ -> false

        new(v : uint64) =
            {
                B0 = (byte (v &&& 255UL))
                B1 = (byte ((v >>> 8) &&& 255UL))
                B2 = (byte ((v >>> 16) &&& 255UL))
            }

        new(v : uint32) =
            {
                B0 = (byte (v &&& 255u))
                B1 = (byte ((v >>> 8) &&& 255u))
                B2 = (byte ((v >>> 16) &&& 255u))
            }

        new(v : uint16) =
            {
                B0 = (byte (v &&& 255us))
                B1 = (byte ((v >>> 8) &&& 255us))
                B2 = 0uy
            }

        new(v : uint8) =
            {
                B0 = v
                B1 = 0uy
                B2 = 0uy
            }

        new(v : int8) = uint24(uint8 v)
        new(v : int16) = uint24(uint16 v)
        new(v : int32) = uint24(uint32 v)
        new(v : int64) = uint24(uint64 v)

        new (v : float32) = uint24(uint32 v)
        new (v : float) = uint24(uint32 v)
        new (v : decimal) = uint24(uint32 v)
        new (v : char)  = uint24(uint16 v)
        new (v : nativeint) = uint24(uint32 v)
        new (v : unativeint) = uint24(uint32 v)
    end

and uint24 = UInt24

[<Struct; StructuredFormatDisplay("{AsString}"); StructuralEquality; NoComparison>]
type Depth24Stencil8 =
    val mutable private _depth : uint24
    val mutable private _stencil : uint8


    
    member x.Depth
        with get() = 
            float32 x._depth / 16777215.0f
        and set (v : float32) =
            x._depth <- uint24 (16777215.0f * (max 0.0f (min 1.0f v)))

    member x.Stencil
        with get() = x._stencil
        and set v = x._stencil <- v
    
    member private x.AsString = x.ToString()
    override x.ToString() = sprintf "[%f, %d]" x.Depth x.Stencil

    static member op_Explicit(x : Depth24Stencil8) : uint32 = (uint32 x._depth <<< 8) ||| uint32 x._stencil
    static member op_Explicit(x : Depth24Stencil8) : uint64 = (uint64 x._depth <<< 8) ||| uint64 x._stencil
    static member op_Explicit(x : Depth24Stencil8) : float = float x.Depth
    static member op_Explicit(x : Depth24Stencil8) : float32 = x.Depth

    new (depth : float32, stencil : uint8) =
        { 
            _depth = uint24 (16777215.0f * (max 0.0f (min 1.0f depth)))
            _stencil = stencil
        }
    new (depth : float, stencil : uint8) =
        { 
            _depth = uint24 (16777215.0 * (max 0.0 (min 1.0 depth)))
            _stencil = stencil
        }

    new(v : uint32) =
        {
            _depth = uint24 (v >>> 8)
            _stencil = uint8 v
        }

[<AutoOpen>]
module Operators = 
    let inline private convAux< ^a, ^b when (^a or ^b) : (static member op_Explicit : ^a -> ^b)> (a : ^a) (b : ^b) =
        ((^a or ^b) : (static member op_Explicit : ^a -> ^b) (a))
    
    let inline uint24 a : uint24 = convAux a Unchecked.defaultof<UInt24>


#r "nuget: FParsec, 1.1.1"
open FParsec

let hexToBin (c: char) =
    let s = string c
    let x = System.Convert.ToInt32(s, 16)
    System.Convert.ToString(x, 2)

let rawBinaryInput = 
    System.IO.File.ReadAllText("input.txt")
    |> (fun s -> s.ToCharArray())
    |> Array.map hexToBin
    |> System.String.Concat

type PacketVersion = int
type PacketType =
    | LiteralValue of int
    static member Create (t, v) =
        match t with
        | 4 -> LiteralValue v
        | x -> failwithf "Unknown packet type %i" x

let bytesToNumber (bs: string list) =
    let x = System.String.Concat bs
    System.Convert.ToInt32(x, 2)

let pPaket =
    let zero = pstring "0"
    let one = pstring "1"
    let bit = choice [zero; one]
    let byte = 
        bit .>>. bit .>>. bit .>>. bit
        |>> (fun (((a,b),c),d) -> System.String.Concat [a;b;c;d])
    let pVersion = 
        bit .>>. bit .>>. bit
        |>> (fun ((a,b),c) -> System.String.Concat [a;b;c])
        |>> (fun s -> System.Convert.ToInt32(s, 2))
    let pType =
        let typePrefix = pVersion // also a 3-bit integer
        let bytePacks = 
            let onePack = one >>. byte
            let zeroPack = zero >>. byte
            many onePack .>>. zeroPack
            |>> (fun (bs,b) -> bytesToNumber (bs @ [b]))
        typePrefix .>>. bytePacks
        |>> PacketType.Create
    let zeroTail = many zero
    pVersion .>>. pType .>> zeroTail

run pPaket "110100101111111000101000"
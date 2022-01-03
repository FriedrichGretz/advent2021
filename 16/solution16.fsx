#r "nuget: FParsec, 1.1.1"
open FParsec

let hexToBin (c: char) =
    let s = string c
    let x = System.Convert.ToInt32(s, 16)
    let bin = System.Convert.ToString(x, 2)
    String.replicate (4 - bin.Length) "0"
    + bin

let toBinary (str: string) =
    str
    |> (fun s -> s.Trim())
    |> (fun s -> s.ToCharArray())
    |> Array.map hexToBin
    |> System.String.Concat

let rawBinaryInput = 
    System.IO.File.ReadAllText("input.txt")
    |> toBinary

type PacketVersion = int64
type PacketType =
    | Sum
    | Product
    | Min
    | Max
    | LiteralValue
    | Gt
    | Lt
    | Eq
    static member Create x =
        match x with
        | 0L -> Sum
        | 1L -> Product
        | 2L -> Min
        | 3L -> Max
        | 4L -> LiteralValue
        | 5L -> Gt
        | 6L -> Lt
        | 7L -> Eq
        | _ -> failwith "unknown Operator"
    // | LiteralValue of int
    // static member Create (t, v) =
    //     match t with
    //     | 4 -> LiteralValue v
    //     | x -> failwithf "Unknown packet type %i" x

type PacketContents =
    | LiteralPack of int64
    | SumPack of Packet array
    | ProductPack of Packet array
    | MinPack of Packet array
    | MaxPack of Packet array
    | GtPack of Packet array
    | LtPack of Packet array
    | EqPack of Packet array
    

and Packet =
    {
        version: PacketVersion
        contents: PacketContents
    }
    member this.VersionSum =
        match this.contents with
        | LiteralPack _ -> 0L
        | SumPack packets
        | ProductPack packets
        | MinPack packets
        | MaxPack packets
        | GtPack packets
        | LtPack packets
        | EqPack packets -> packets |> Array.sumBy (fun p -> p.VersionSum)
        + this.version
    member this.Eval =
        match this.contents with
        | LiteralPack v -> v
        | SumPack packets -> packets |> Array.sumBy (fun p -> p.Eval)
        | ProductPack packets -> packets |> Array.fold (fun product p -> product * p.Eval) 1
        | MinPack packets -> packets |> Array.map (fun p -> p.Eval) |> Array.min
        | MaxPack packets -> packets |> Array.map (fun p -> p.Eval) |> Array.max
        | GtPack packets -> if packets[0].Eval > packets[1].Eval then 1 else 0
        | LtPack packets -> if packets[0].Eval < packets[1].Eval then 1 else 0
        | EqPack packets -> if packets[0].Eval = packets[1].Eval then 1 else 0


type SubPacketNum =
    | TotalBits of int64
    | Count of int64

let bytesToNumber (bs: string seq) =
    let x = System.String.Concat bs
    System.Convert.ToInt64(x, 2)

/// https://stackoverflow.com/a/70569222/2289899
let manyLimit nChars p =
    parse {
        let! startPos = getPosition

        let rec loop values =
            parse {
                let! curPos = getPosition
                let nRemain = (startPos.Index + nChars) - curPos.Index
                if nRemain = 0 then
                    return values
                elif nRemain > 0 then
                    let! value = p
                    return! loop (value :: values)
                else
                    return! fail $"limit exceeded by {-nRemain} chars"
            }

        let! values = loop []
        return values |> List.rev |> Array.ofList
    }

let zero = pstring "0"
let one = pstring "1"
let bit = choice [zero; one]
let byte = 
    parray 4 bit
    |>> System.String.Concat

let pVersion = 
    parray 3 bit
    |>> bytesToNumber

let pTotalSubPackLength =
    zero >>. parray 15 bit
    |>> (bytesToNumber >> TotalBits)

let pTotalSubPacksCount =
    one >>. parray 11 bit
    |>> (bytesToNumber >> Count)

let rec pOps =
    choice [pTotalSubPackLength; pTotalSubPacksCount]
    >>= function
        | TotalBits tb -> 
            manyLimit tb pPaket
        | Count c -> 
            parray ((int)c) pPaket
and pPaket =
    let pType =
        let typePrefix = 
            pVersion // also a 3-bit integer
            |>> PacketType.Create
        let bytePacks = 
            let onePack = one >>. byte
            let zeroPack = zero >>. byte
            many onePack .>>. zeroPack
            |>> (fun (bs,b) -> bytesToNumber (bs @ [b]))
        typePrefix 
        >>= function
            | LiteralValue -> bytePacks |>> LiteralPack
            | Sum -> pOps |>> SumPack
            | Product -> pOps |>> ProductPack
            | Min -> pOps |>> MinPack
            | Max -> pOps |>> MaxPack
            | Gt -> pOps |>> GtPack
            | Lt -> pOps |>> LtPack
            | Eq -> pOps |>> EqPack
    pVersion .>>. pType
    |>> fun (v, c) -> {version = v; contents = c} 

let pWholeMsg = 
    let zeroTail = many zero
    pPaket .>> zeroTail

// run pWholeMsg "110100101111111000101000"
// run pWholeMsg "11101110000000001101010000001100100000100011000001100000"
// toBinary "D2FE28"
// toBinary "8A004A801A8002F478"
// |> run pWholeMsg

run pWholeMsg rawBinaryInput
|> function
    | Success (packet, _, _) ->
        printfn "Packet structure %A" packet
        printfn "Sum of versions %i" packet.VersionSum
        printfn "Evaluated value %i" packet.Eval
    | Failure (msg,perror,state) -> failwithf "%s \n %A \n %A" msg perror state
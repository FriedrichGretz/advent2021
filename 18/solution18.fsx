#r "nuget: FParsec, 1.1.1"
open FParsec

type SnailNumber =
    {
        f: NumOrPair
        s: NumOrPair
    }
    override this.ToString() =
        sprintf "[%s, %s]" (this.f.ToString()) (this.s.ToString())

and NumOrPair =
    | Num of int
    | Pair of SnailNumber
    override this.ToString() =
        match this with
        | Num x -> string x
        | Pair sn -> sn.ToString()


let rec addRightmost numOrPair valueToAdd =
    match numOrPair with
    | Num n -> Num (n + valueToAdd)
    | Pair {f=f;s=s} -> Pair {f=f; s=addRightmost s valueToAdd}

let rec addLeftmost numOrPair valueToAdd =
    match numOrPair with
    | Num n -> Num (n + valueToAdd)
    | Pair {f=f;s=s} -> Pair {f=addLeftmost f valueToAdd; s=s}

let tryExplode snailNum =
    let rec explode depth left cur right =
        match cur, depth with
        | Num n, _ -> Result.Error () // already arrived at sigle number, but we need a pair at depth 4
        | Pair {f=Num f;s= Num s}, 4 -> 
            let newLeft = 
                left
                |> Option.map (fun someL -> addRightmost someL f)
            let newRight =
                right
                |> Option.map (fun someR -> addLeftmost someR s)
            Result.Ok (newLeft, (Num 0), newRight)
        | Pair {f=f;s=s}, _ -> 
            assert (depth < 4)
            match explode (depth + 1) left f (Some s) with
            | Result.Ok (newL,newC,Some newR) ->
                Result.Ok (newL, Pair {f=newC; s=newR}, right)
            | Result.Ok (newL,newC,None) ->
                failwith "Cannot happen, we have put (Some s) into the right context."
            | Result.Error _ ->
                match explode (depth + 1) (Some f) s right with
                | Result.Ok (Some newL,newC,newR) ->
                    Result.Ok (left, Pair {f=newL; s=newC}, newR)
                | Result.Ok (None,newC,newR) ->
                    failwith "Cannot happen, we have put (Some f) into the left context."
                | Result.Error x -> Result.Error x
    explode 0 None (Pair snailNum) None
    |> function
        | Result.Ok (None, Pair sn, None) -> Result.Ok sn
        | Result.Ok (_,_,_) -> failwith "Cannot happen, we have put None,pair,None in, must get None,pair,None out."
        | Result.Error x -> Result.Error x

/// Find the left most number >= 10 and split it into a SnailNumber
let trySplit snailNum =
    let rec split numOrPair =
        match numOrPair with
        | Num n when (n < 10) -> Result.Error (Num n)
        | Num n -> Result.Ok (Pair { f = Num (n/2)
                                     s = Num (n - n/2) })
        | Pair {f=f;s=s} ->
            match split f with
            | Result.Ok x -> Result.Ok (Pair {f=x;s=s})
            | Result.Error x ->
                match split s with
                    | Result.Ok x -> Result.Ok (Pair {f=f;s=x})
                    | Result.Error x -> Result.Error (Pair {f=f;s=s})
    split (Pair snailNum)
    |> function
        | Result.Ok (Pair sn) -> Result.Ok sn
        | Result.Ok _ -> failwith "Cannot happen, we have put a Pair sn in, must get Pair sn out."
        | Result.Error x -> Result.Error x

let rec reduceSN n =
    match tryExplode n with
    | Result.Ok newN -> reduceSN newN
    | Result.Error _ ->
        match trySplit n with
        | Result.Ok newN -> reduceSN newN
        | Result.Error _ -> n

let addSN a b =
    { f = Pair a
      s = Pair b }
    |> reduceSN

let magnitude sn =
    let rec mag numOrPair =
        match numOrPair with
        | Num n -> n
        | Pair {f=f;s=s} -> 3 * mag f + 2 * mag s
    mag <| Pair sn

let pSnailNumber, pSnailNumberRef = createParserForwardedToRef<SnailNumber, unit>()
let pIntOrSnail =
    pint32 |>> Num
    <|> (pSnailNumber |>> Pair)
do pSnailNumberRef.Value <-
    pstring "[" 
    >>. pIntOrSnail 
    .>> pstring "," 
    .>>. pIntOrSnail 
    .>> pstring "]"
    |>> (fun (f,s) -> {f=f;s=s})

// let testSnail = 
//     //"[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
//     "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
//     |> run pSnailNumber 
//     |> function 
//         | Success (res,_,_) -> res
//         | Failure (msg,_,_) -> failwithf "%s" msg

// testSnail
// |> reduceSN
// |> (fun n -> n.ToString() |> printfn "%A")

let numbers = 
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (run pSnailNumber >> function | Success (r,_,_) -> r | Failure _ -> failwith "Failed parsing")

numbers
|> Array.reduce addSN
|> magnitude
|> printfn "Part A. Magnitude of sum: %i"

let allPairs =
    numbers 
    |> Array.collect (fun sn1 ->
        numbers |> Array.map (fun sn2 -> addSN sn1 sn2))
allPairs
|> Array.map magnitude
|> Array.max
|> printfn "Part B. Highest magnitude is: %i"
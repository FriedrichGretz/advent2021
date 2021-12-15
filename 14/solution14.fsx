#r "nuget: FParsec, 1.1.1"
open FParsec
let ws = spaces
let arrow_ws = pstring "->" .>> ws
let rule = 
    identifier (IdentifierOptions()) .>> ws
    .>> arrow_ws
    .>>. identifier (IdentifierOptions()) .>> ws
let parseRules = many1 rule
let parseTemplate = identifier (IdentifierOptions()) .>> ws
let parseFile = parseTemplate .>>. parseRules

let template, rules =
    System.IO.File.ReadAllText("input.txt")
    |> run parseFile
    |> function
        | Success (x,_,_) ->
            //printfn "%A" x
            fst x, snd x |> List.fold (fun m (k,v) -> Map.add k v m) Map.empty
        | Failure (errorAsString, error, suserState) -> failwithf "invalid input %A" errorAsString

let pairs = 
    template.ToCharArray()
    |> Array.windowed 2
    |> Array.map (fun x -> System.String x, 1UL)
// printfn "%A" pairs

let addToMap elems map =
    let update increment = function
        | Some count -> Some (count + increment)
        | None -> Some increment
    elems
    |> Seq.fold (fun m (e,increment) -> Map.change e (update increment) m) map
let initMap = addToMap pairs Map.empty
// printfn "%A" initMap

// TODO this is wrong. NC -> 2 needs to not only insert NB BC once but twice!! Apllying rules and counting cannot be done separately
let applyRulesToAll (map: Map<_,_>) =
    let insert (s: string) (l: string) =
        assert (s.Length = 2)
        assert (l.Length = 1)
        let ca = s.ToCharArray()
        [| ca[0]
           l.ToCharArray()[0]
           ca[1] |]
    let applyRules key =
        let count = map.[key]
        match Map.tryFind key rules with
        | None -> []
        | Some letter ->
            insert key letter
            |> Array.windowed 2
            |> Array.map (fun x -> System.String x, count)
            |> Array.toList
    map.Keys |> Seq.collect applyRules
// printfn "%A" <| applyRulesToAll initMap.Keys

let applyAndAdd (map: Map<_,_>) =
    applyRulesToAll map
    |> addToMap <| Map.empty

let rec fixn f n x =
    if n = 0 then x
    else fixn f (n - 1) (f x)

let map10 = fixn applyAndAdd 10 initMap
printfn "After 10 iterations."
map10 |> Map.iter (printfn "%s -> %i")

let countLetters map =
    let lastLetter =
        let ca = template.ToCharArray()
        ca[ca.Length-1]
    map 
    |> Map.toList
    |> List.map (fun (k: string, v: uint64) ->
        let ca = k.ToCharArray()
        ca[0], v
        //  ca[1], v ]
        )
    |> List.groupBy fst
    |> List.map (fun (grp, lst) -> grp, lst |> List.sumBy snd)
    |> List.map (fun (grp, count) -> if grp = lastLetter then (grp, count + 1UL) else (grp,count)) // add last letter in template

let counts = countLetters map10
printfn "%A" counts
let _,maxcount = Seq.maxBy snd counts
let _,mincount = Seq.minBy snd counts 
maxcount - mincount |> printfn "Part A. Most common - least common = %i"

let map40 = fixn applyAndAdd 30 map10
printfn "After 30 more iterations."
let countsB = countLetters map40
printfn "%A" countsB
let _,maxcountB = Seq.maxBy snd countsB
let _,mincountB = Seq.minBy snd countsB 
maxcountB - mincountB |> printfn "Part A. Most common - least common = %i"
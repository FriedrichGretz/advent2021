let canonicalForm (s: string) = 
    s.ToCharArray()
    |> Array.sort

let updateMapping mapping charr =
    let isIn a1 a2 =
        a1 |> Array.forall (fun i -> Array.contains i a2)
    let keyOf i =
        mapping |> Map.findKey (fun _ v -> v = i)
    let union a1 a2 =
        Array.concat [|a1;a2|]
        |> Array.distinct
    if not (Map.containsKey charr mapping) then
        if Array.length charr = 2 then
            // charr represents 1
            mapping |> Map.add charr 1
        elif Array.length charr = 3 then
            // charr represents 7
            mapping |> Map.add charr 7
        elif Array.length charr = 4 then
            // charr represents 4
            mapping |> Map.add charr 4
        elif Array.length charr = 7 then
            // charr represents 8
            mapping |> Map.add charr 8
        elif Array.length charr = 5 then
            // charr represents 2, 3, 5
            if isIn (keyOf 1) charr then // 3
                mapping |> Map.add charr 3
            elif isIn (keyOf 8) (union charr (keyOf 4)) then // 2
                mapping |> Map.add charr 2
            else // 5
                mapping |> Map.add charr 5
        else
            assert (Array.length charr = 6)
            // charr represents 6, 9
            if isIn (keyOf 1) charr then // 9
                mapping |> Map.add charr 9
            else // 6
                mapping |> Map.add charr 6
    else
        mapping

let createMapping observedDigits =
    let entropy (digit: char[]) = digit.Length % 7
    observedDigits
    |> Array.sortBy entropy
    |> Array.fold updateMapping Map.empty

let lines =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> s.Split "|" |> Array.map(fun s -> s.Trim().Split " " |> Array.map canonicalForm))
    
let decodedDigits = 
    lines
    |> Array.map(fun line ->
        let m = createMapping line[0]
        line[1] |> Array.map (fun x -> m.[x])
        )

let count1478 counter digit =
    if List.contains digit [1; 4; 7; 8] then counter + 1
    else counter

decodedDigits
|> Array.concat
|> Array.fold count1478 0
|> printfn "Part A: a total of %i occurrences of 1, 4, 7 or 8."

let digitsToNum fourDigits =
    assert (Array.length fourDigits = 4)
    1000 * fourDigits[0] + 100 * fourDigits[1] + 10 * fourDigits[2] + fourDigits[3]

let numbers =
    decodedDigits
    |> Array.map digitsToNum
numbers |> Array.iter (printfn "%A")
numbers |> Array.sum |> printfn "Part B: total sum %i."
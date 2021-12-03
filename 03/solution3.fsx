let digitsLines = 
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> s.ToCharArray())


let maskDigits (digitsLines: 'a[][]) =
    [
        for i in 0 .. digitsLines[0].Length - 1 ->
            1 <<< i
    ]
    |> List.sum
    |> uint32


let inline charToInt c = int c - int '0'


let mostFrequentDigit digits =
    let numOfOnes =
        digits
        |> Array.map charToInt
        |> Array.sum 
    if (float)numOfOnes >= ceil((float)digits.Length/2.0) then '1'
    else '0'

let invert c =
    if c = '1' then '0'
    else '1'


let isCharInPos (digit: char) pos (arr: char array) =
    arr[pos] = digit


let resultA =
    let matrix = array2D digitsLines 
    let gamma =
        [|
            for i in 0..matrix[0,*].Length-1 ->
                matrix[*,i] // i-th column
                |> mostFrequentDigit
        |]  
        |> System.String.Concat
        |> (fun v -> System.Convert.ToUInt32(v, 2))  
    let epsilon = gamma ^^^ (maskDigits digitsLines)
    gamma, epsilon


let resultB =
    
    let mostFrequentDigit pos lines =
        let matrix = array2D lines 
        matrix[*,pos] // i-th column
        |> mostFrequentDigit
        
    let leastFrequentDigits pos lines = 
        mostFrequentDigit pos lines
        |> invert

    let rec filterOut filterFun (remainingLines: char[][]) pos =
        if remainingLines.Length > 1 && pos < remainingLines[0].Length then
            let digit = filterFun pos remainingLines
            let newRemainder =
                remainingLines
                |> Array.filter (isCharInPos digit pos)
            filterOut filterFun newRemainder (pos + 1)
        else
            remainingLines, pos

    let oxygenGeneratorRating, _ = 
        (filterOut mostFrequentDigit) digitsLines 0

    let CO2ScrubberRating, _ =
        (filterOut leastFrequentDigits) digitsLines 0
        
    assert (oxygenGeneratorRating.Length = 1)
    assert (CO2ScrubberRating.Length = 1)
    printfn "%A" oxygenGeneratorRating
    printfn "%A" CO2ScrubberRating
    let a = 
        oxygenGeneratorRating[0]
        |> System.String.Concat
        |> (fun v -> System.Convert.ToUInt32(v, 2))
    let b =
        CO2ScrubberRating[0]
        |> System.String.Concat
        |> (fun v -> System.Convert.ToUInt32(v, 2))
    a,b

printfn "Result A: %A, multiplied: %A" resultA (fst resultA * snd resultA)
printfn "Result B: %A, multiplied: %A" resultB (fst resultB * snd resultB)
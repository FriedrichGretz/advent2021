#r "nuget: FParsec, 1.1.1"
open FParsec

let chunk, chunkRef = createParserForwardedToRef<string, unit>()

let parens = 
    pstring "(" .>> (many chunk) >>. pstring ")"
let brackets =
    pstring "[" .>> many chunk >>. pstring "]"
let braces =
    pstring "{" .>> many chunk >>. pstring "}"
let angles =
    pstring "<" .>> many chunk >>. pstring ">"
do chunkRef.Value <-
    choice [
        parens
        brackets 
        braces  
        angles  
    ]

let scoreOfToken t =
    if t = ')' then 3
    elif t = ']' then 57
    elif t = '}' then 1197 
    elif t = '>' then 25137
    else failwithf "Unexpected token %c" t


let processRowResult (input: string) parserResult =
    match parserResult with
    | Success _ -> 
        printfn "parsed row succefully"
        0
    | Failure (msg, err, state) ->
        printfn "%s" msg
        let col = (int)err.Position.Column - 1 // position is 1 based
        if col < input.Length then
            let score = scoreOfToken input[col]
            printfn "Corrupted line with symbol %c, score %i" input[col] score
            score
        else
            printfn "Incomplete line"
            0

let lines = System.IO.File.ReadAllLines("input.txt")

lines
|> Array.map (fun line -> run chunk line |> processRowResult line)
|> Array.sum
|> printfn "Part A. Sum of scores %i"


let filterPossibleClosingToken (err: ErrorMessage) =
    match err with 
    | :? ExpectedString as expectedStr -> 
        let token = expectedStr.String
        if List.contains token [")";"]";"}";">"] then
            Some token
        else None
    | _ -> None


let scoreClosing t =
    if t = ")" then 1L
    elif t = "]" then 2L
    elif t = "}" then 3L 
    elif t = ">" then 4L
    else failwithf "Unexpected token %s" t

open System.Diagnostics

let rec tryRepairRow (input: string) score parserResult =
    match parserResult with
    | Success _ -> Some score
    | Failure (msg, err, state) ->
        printfn "%s" msg
        let col = (int)err.Position.Column - 1 // position is 1 based
        if col < input.Length then None // corrupted line
        else // incomplete line
            let closingTokens =
                ErrorMessageList.ToSortedArray err.Messages
                |> Array.choose filterPossibleClosingToken
            Debug.Assert(closingTokens.Length = 1, "Expected only one closing token.")
            let token = closingTokens[0]
            let newScore = 5L * score + (scoreClosing token)
            let newInput = input + token
            run chunk newInput
            |> tryRepairRow newInput newScore

// Testing        
// let input =
//     //"[<>({}){}[([])<>]]" // ok
//     "[({(<(())[]>[[{[]{<()<>>" // incomplete
//     //"{([(<{}[<>[]}>{[]{[(<()>" // corrupt
// run chunk input
// |> tryRepairRow input 0L
// |> printfn "%A"

lines
|> Array.choose (fun line -> run chunk line |> tryRepairRow line 0L)
|> Array.sort
|> (fun arr -> printfn "Part B. Middle repair score %i" arr[arr.Length/2])
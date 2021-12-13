#r "nuget: FParsec, 1.1.1"

type Coord =
    {
        x: int
        y: int
    }

type FoldInstruction =
    | Left of int // x = ...
    | Up of int // y = ...

open FParsec
let ws = spaces
let comma_ws = pstring "," .>> ws
let fold_ws = pstring "fold along" .>> ws
let int_ws = pint32 .>> ws
let axis = 
    choice [
        pstring "x=" >>. int_ws |>> Left
        pstring "y=" >>. int_ws |>> Up
        ]
let parseCoord = 
    int_ws .>> comma_ws .>>. int_ws
    |>> (fun (x,y) -> {x=x;y=y})
let parseFold = fold_ws >>. axis 
let parseFile = 
    many1 parseCoord 
    .>>. many1 parseFold

let coords, instructions =
    System.IO.File.ReadAllText("input.txt")
    |> run parseFile
    |> function
        | Success (x,_,_) ->
            //printfn "%A" x
            fst x, snd x
        | Failure _ -> failwith "invalid input"

let foldPaper cs instruction =
    let transformLeft pos c =
        if c.x > pos then {x = 2 * pos - c.x; y = c.y}
        else c
    let transformUp pos c =
        if c.y > pos then {x = c.x; y = 2 * pos - c.y}
        else c
    let transform =
        match instruction with
        | Up pos -> transformUp pos
        | Left pos -> transformLeft pos
    cs
    |> List.map transform
    |> List.distinct

System.Console.Clear();
foldPaper coords instructions[0]
|> List.length
|> printfn "Part A. Dots after one fold: %i"

let visualise cs =
    printfn "Part B."
    let origRow = System.Console.CursorTop;
    let origCol = System.Console.CursorLeft;
    let write c =
        System.Console.SetCursorPosition(c.x, 10 + c.y);
        System.Console.Write("#");
    cs |> List.iter (write)
    System.Console.SetCursorPosition(origRow, origCol + 20);


instructions
|> List.fold foldPaper coords
|> visualise

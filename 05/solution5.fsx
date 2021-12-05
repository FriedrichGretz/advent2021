#r "nuget: FParsec, 1.1.1"

type Point = {x: int; y: int}
type Line = 
    {
        start: Point
        end_: Point
    }


let enumerate num1 num2 =
    if num1 <= num2 then [num1 .. num2]
    else [num1 .. -1 .. num2]
let enumVert x y1 y2 =
    enumerate y1 y2
    |> List.map (fun y -> {x = x; y = y})
let enumHor x1 x2 y =
    enumerate x1 x2
    |> List.map (fun x -> {x = x; y = y})

let generatePointsFromLine_A (line: Line) =
    if line.start.x = line.end_.x then
        enumVert line.start.x line.start.y line.end_.y
    elif line.start.y = line.end_.y then
        enumHor line.start.x line.end_.x line.start.y
    else
        // ignore other lines
        []

let generatePointsFromLine_B (line: Line) =
    if line.start.x = line.end_.x then
        enumVert line.start.x line.start.y line.end_.y
    elif line.start.y = line.end_.y then
        enumHor line.start.x line.end_.x line.start.y
    else
        // must be diagonal
        List.map2 (fun x y -> {x = x; y = y})
            <| enumerate line.start.x line.end_.x
            <| enumerate line.start.y line.end_.y


type VentMap = Map<Point, int>


let addPointToMap ventMap point =
    let update = function
        | Some count -> Some (count + 1)
        | None -> Some 1
    Map.change point update ventMap


let countDangerousPoints ventMap =
    let above2 _ v = v >= 2
    Map.filter above2 ventMap
    |> Map.count


open FParsec
let parseLine =
    let ws = spaces
    let comma_ws = pstring "," .>> ws
    let arrow_ws = pstring "->" .>> ws
    let int_ws = pint32 .>> ws
    int_ws .>> comma_ws .>>. int_ws .>> arrow_ws .>>. int_ws .>> comma_ws .>>. int_ws
    |>> fun (((a,b),c),d) -> {start = {x = a; y = b}; end_ = {x = c; y = d}}
    
let parseAll =
    many1 parseLine

let lineList = 
    System.IO.File.ReadAllText("input.txt")
    |> run parseAll 
    |> function
        | Success (x,_,_) -> x
        | Failure _ -> failwith "invalid input"


let collectPointsAndCount generatePointsFromLine =
    lineList
    |> List.collect generatePointsFromLine
    |> List.fold addPointToMap Map.empty
    |> countDangerousPoints
    |> printfn "Found %A dangerous points."

printfn "Solution A"
collectPointsAndCount generatePointsFromLine_A

printfn "Solution B"
collectPointsAndCount generatePointsFromLine_B
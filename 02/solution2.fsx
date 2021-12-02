type Instruction =
    | Fwd of int
    | Dwn of int
    | Up of int

let parseCmd text amount =
    match text with
    | "forward" -> Fwd amount
    | "down" -> Dwn amount
    | "up" -> Up amount
    | _ -> failwith "unknown command"
    
let parseInstructions (instr: string) =
    let pair = instr.Trim().Split([|' '|])
    let cmd = pair[0]
    let amount = (int)pair[1]
    parseCmd cmd amount

let navigate state cmd =
    match cmd with
    | Fwd x -> (fst state + x, snd state)
    | Dwn x -> (fst state, snd state + x)
    | Up x -> (fst state, snd state - x)

let aimNavigate state cmd =
    let (hor, depth, aim) = state
    match cmd with
    | Fwd x -> (hor + x, depth + x * aim, aim)
    | Dwn x -> (hor, depth, aim + x)
    | Up x -> (hor, depth, aim - x)

let instr = 
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map parseInstructions

let coord = 
    instr
    |> Array.fold navigate (0,0)

let coord2 =
    instr
    |> Array.fold aimNavigate (0,0,0)

printfn "Solution A: Final coordinate is %A. The answer is %i." coord (fst coord * snd coord)
let (hor, depth, _) = coord2
printfn "Solution B: Final coordinate is %A. The answer is %i." coord2 (hor * depth)
// Board is 2D array
type Board = (int * bool)[,]


let update board number = 
    for r in 0 .. Array2D.length1 board - 1 do
        for c in 0 .. Array2D.length2 board - 1 do
            if fst board[r,c] = number then
                board[r, c] <- (number, true)
    board


let checkWin board =
    let check arr = Array.forall snd arr
    let rec checkBoard pos =
        if pos >= Array2D.length1 board then false
        elif check board[*,pos] then true
        elif check board[pos,*] then true
        else checkBoard (pos + 1)
    let sumBoard =
        let mutable sum = 0
        let addIfTrue (v,t) =
            sum <- sum + if not t then v else 0
        Array2D.iter addIfTrue board
        sum
    if checkBoard 0 then
        Some sumBoard
    else
        None


let rec updateSeq (numbers: int[]) moves board =
    // Array.tryPick (update board >> checkWin) numbers
    if numbers.Length = 0 then
        None, moves
    else
        match update board numbers[0] |> checkWin with
        | Some x -> Some (x * numbers[0]), moves
        | None ->
            updateSeq numbers[1..] (moves + 1) board 


let parseSequence (s: string) =
    s.Split ','
    |> Array.map int


let removeEmpty a = a |> Array.filter (fun x -> not (System.String.IsNullOrWhiteSpace x))


let parseBoard (s: string) =
    let rows = s.Split '\n'    
    let asBoardRow r = r |> Array.map (fun item -> (int item, false))
    rows
    |> Array.map (fun r -> r.Split ' ' |> removeEmpty |> asBoardRow)
    |> array2D

let chunks = System.IO.File.ReadAllText("input.txt").Split "\n\n"
let numberSequence = parseSequence chunks[0]
let boards = Array.map parseBoard chunks[1..]

let allPlayed = 
    boards
    |> Array.map (updateSeq numberSequence 0)

allPlayed
|> Array.minBy (snd)
|> printfn "Earliest to win %A"

allPlayed
|> Array.maxBy (snd)
|> printfn "Latest to win %A"
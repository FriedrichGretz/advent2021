let input = 
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> System.Int32.Parse))
    |> array2D
let mutable numOfLows = 0
let mutable riskLevels = 0

let enumarateNeighbours x y table =
    [
        if x > 0 then Some (x - 1, y) else None
        if x < Array2D.length1 table - 1 then Some (x + 1, y) else None
        if y > 0 then Some (x, y - 1) else None
        if y < Array2D.length2 table - 1 then Some (x, y + 1) else None
    ]
    |> List.choose id

let addLowPoint x y value =
    let check (a, b) =
        input[a,b] > value
    enumarateNeighbours x y input
    |> List.map check
    |> List.forall id
    |> function
        | true ->
            riskLevels <- (riskLevels + value + 1)
            numOfLows <- numOfLows + 1
        | false -> ()
        

input |> Array2D.iteri addLowPoint
printfn "Part A: Number of low points: %i" numOfLows
printfn "Part A: Sum of risk levels: %u" riskLevels

let map: (int * bool)[,] = 
    Array2D.map (fun i -> i, false) input

let rec visit (x, y) =
    let value, isVisited = map[x,y]
    if isVisited then 0
    else
        map[x,y] <- (value, true) // mark as visited
        if value = 9 then
            0
        else
            let visitNeighbours =
                enumarateNeighbours x y map
                |> List.sumBy visit
            1 + visitNeighbours

let basinSizes = ResizeArray<int>()
for r in 0 .. Array2D.length1 map - 1 do
    for c in 0 .. Array2D.length2 map - 1 do
        if not (snd map[r,c]) then
            basinSizes.Add(visit(r,c))

basinSizes.Sort()
basinSizes.Reverse()
printfn "%A" basinSizes
printfn "Part B: product over top 3 = %A" (basinSizes[0] * basinSizes[1] * basinSizes[2])
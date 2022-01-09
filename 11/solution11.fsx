let increase grid =
    grid |> Array2D.map (fun i -> i + 1)

let resetToZero grid =
    grid |> Array2D.map (fun i -> if i = -1 then 0 else i)

let increaseNeighbours grid x y =
    for r in x - 1 .. x + 1 do
        for c in y - 1 .. y + 1 do
            if r >= 0 
               && r <= Array2D.length1 grid - 1
               && c >= 0
               && c <= Array2D.length2 grid - 1
               && (r,c) <> (x,y)
               && grid[r,c] <> -1 then
                grid[r,c] <- grid[r,c] + 1

let flashCell (grid: int[,]) r c =
    // energy level 0 .. 9
    // flash if >=10
    // -1 means has flashed in this step
    if grid[r,c] >= 10 then //flash!
        grid[r,c] <- -1
        increaseNeighbours grid r c
        1
    else 
        0

let rec flash flashesSoFar grid =
    let mutable numOfFlashes = flashesSoFar
    for r in 0 .. Array2D.length1 grid - 1 do
        for c in 0 .. Array2D.length2 grid - 1 do
            numOfFlashes <- numOfFlashes + flashCell grid r c
    if numOfFlashes > flashesSoFar then flash numOfFlashes grid
    else grid, numOfFlashes

let step grid  =
    let gridAfterFlash, flashCount =
        increase grid
        |> flash 0
    gridAfterFlash |> resetToZero, flashCount

let grid = 
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> System.Int32.Parse))
    |> array2D

// Part A
printfn "%A" grid
[1..100] 
|> List.fold (fun (g,f) c -> 
    let res, flashCount = step g
    let totalFlashes = flashCount + f
    //printfn "Grid after step %i \n%A" c res
    printfn "%i flashes" totalFlashes
    res, totalFlashes
    ) (grid, 0)

// Part B
let rec waitForAllFlash g f s =
    let res, flashCount = step g
    let totalFlashes = flashCount + f
    let totalSteps = s + 1
    if flashCount = 100 then
        totalSteps
    else 
        waitForAllFlash res totalFlashes totalSteps

waitForAllFlash grid 0 0
|> printfn "Part B. It took %i steps"
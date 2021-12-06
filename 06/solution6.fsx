let fishes =
    System.IO.File.ReadAllText("input.txt")
    |> (fun r -> r.Split ',')
    |> Array.map int
    |> List.ofArray

(* Naive solution for part A *)
let rec procreate oldFishes newFishes =
    match oldFishes with
    | [] -> newFishes
    | f :: fs ->
        if f = 0 then 6 :: 8 :: newFishes
        else (f - 1) :: newFishes
        |> procreate fs

let rec simulate days fishes =
    if days <= 0 then fishes
    else
        procreate fishes []
        |> simulate (days - 1)

simulate 80 fishes
|> List.length
|> printfn "Solution A: %A" 

// clearly this does not scale much farther since not only the run
// time but also the memory usage grows exponentially with the fish

(* Dynamic programming implementation for part B *)

let DAYS = 256
let AGES = 8

let table: int64[,] = Array2D.create (AGES+1) (DAYS+1) 0

let initFishes =
    fishes
    |> List.countBy id
    |> List.sortBy fst

initFishes
|> List.iter (fun (age, count) -> table[age,0]<-count)

let updateTable age day =
    let newValue =
        if age = 8 then
            table[0, day - 1]
        elif age = 6 then
            table[0, day - 1] + table[7, day - 1]
        else
            table[age + 1, day - 1]
    table[age, day] <- newValue

for day in 1..256 do
    for age in 0..8 do
        updateTable age day

let getPopulation day =
    table[*,day] 
    |> Array.sum    

// verify solution A
getPopulation 80
|> printfn "Solution A: %i"

getPopulation 256
|> printfn "Solution B: %i"
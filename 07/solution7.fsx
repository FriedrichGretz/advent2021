let input =
    System.IO.File.ReadAllText("input.txt")
    |> (fun s -> s.Split ",")
    |> Array.map int
    |> Array.countBy id

let minPos,_ = Array.minBy fst input
let maxPos,_ = Array.maxBy fst input

// part A

let distance targetPos (curPos,count) =
    count * abs (targetPos - curPos)

let cummulativeDistance distFun targetPos =
    Array.sumBy (distFun targetPos) input

let calc distFun =
    [
        for i in minPos..maxPos -> i, cummulativeDistance distFun i
    ]
    |> List.minBy snd 

printfn "Part A. targetPos, fuel: %A" <| calc distance
// part B

let distance2 targetPos (curPos,count) =
    let dist = abs (targetPos - curPos)
    count * ((dist * dist + dist)/2)

printfn "Part B. targetPos, fuel: %A" <| calc distance2
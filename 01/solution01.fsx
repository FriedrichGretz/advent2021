let lines = 
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map int


let countIncrements lines =
    let folder (prev,acc) cur =
        if prev < cur then
            (cur, acc + 1)
        else
            (cur, acc)
    let init = System.Int32.MaxValue, 0

    lines
    |> Array.fold folder init
    |> snd


let aggregateTriplets lines =
    let len = Array.length lines
    let lines1 = lines[..len-3]
    let lines2 = lines[1..len-2]
    let lines3 = lines[2..len-1]
    Array.zip3 lines1 lines2 lines3
    |> Array.map (fun (a, b, c) -> a + b + c)    


printf "%A\n" lines
printf "Result A: %A\n" (countIncrements lines)
printf "Result B: %A\n" ((aggregateTriplets >> countIncrements) lines)
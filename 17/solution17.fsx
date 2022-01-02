//input:
let targetXmin = 153
let targetXmax = 199

let targetYmin = -114
let targetYmax = -75

// bound x velocity
let velXmax = targetXmax // we can reach the area in one step
let stepXmin = 1

let velXmin =
    let mutable i = 0
    let mutable lowerBound = targetXmin
    while lowerBound > 0 do // repeat until we reach X origin
        i <- i + 1
        lowerBound <- lowerBound - i
    i
let stepXmax = velXmin

let velYmin = targetYmin
let stepYmin = 1
let velYmax = [1..113] |> List.sum
printfn "Part A. velYmax = %i" velYmax

let simulateStep (xPos, yPos, xVel, yVel) =
    ( xPos + xVel, 
      yPos + yVel,
      max (xVel - 1) 0,
      yVel - 1 )

let rec simulateTrajectory (xPos, yPos, xVel, yVel) =
    if xPos > targetXmax then
        None
    elif yPos < targetYmin then
        None
    elif targetXmin <= xPos && xPos <= targetXmax 
         && targetYmin <= yPos && yPos <= targetYmax then
        Some (xPos, yPos, xVel, yVel)
    else
        simulateStep (xPos, yPos, xVel, yVel)
        |> simulateTrajectory

let possibleTrajectories = 
    [
        for xVel in velXmin .. velXmax do
            for yVel in velYmin .. velYmax do
                match simulateTrajectory (0, 0, xVel, yVel) with
                | Some (finX, finY, _, _) -> yield (finX, finY, xVel, yVel)
                | _ -> ()
    ]

printfn "Part B. Number of trajectories = %A" <| List.length possibleTrajectories
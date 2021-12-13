let adjacencyMap =
    let getVal map key =
        Map.tryFind key map
        |> Option.defaultValue []
    let addSymetrical map key value =
        map
        |> Map.add key (value :: (getVal map key)) 
        |> Map.add value (key :: (getVal map value)) 
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> s.Split "-")
    |> Array.fold (fun state pair -> addSymetrical state pair[0] pair[1]) Map.empty

//printfn "%A" adjacencyMap

let extendPathA path (node: string) =
    if node = node.ToUpper() then // multiple visits OK
        node :: path
    else
        if path |> List.contains node then
            path
        else
            node :: path

let bfs extendPath paths =
    let extendPaths path = 
        if "end" = List.head path then [path]
        else
            let succs = adjacencyMap.[List.head path]
            succs 
            |> List.map (extendPath path)
            |> List.filter (fun newPath -> List.length newPath > List.length path)
    paths |> List.collect extendPaths

let rec fix f x =
    let y = f x
    if y = x then x
    else fix f y

fix (bfs extendPathA) [["start"]]
|> List.length
|> printfn "Path A. Number of paths is %i"

let extendPathB path (node: string) =
    if node = node.ToUpper() then // multiple visits OK
        node :: path
    else
        if node = "start" then
            path
        else
            if path |> List.contains node then
                let smallCaves = path |> List.filter (fun s -> s = s.ToLower())
                if smallCaves = List.distinct smallCaves then // if no repetitions yet, allow one
                    node :: path
                else
                    path
            else
                node :: path

fix (bfs extendPathB) [["start"]]
|> List.length
|> printfn "Path B. Now number of paths is %i"
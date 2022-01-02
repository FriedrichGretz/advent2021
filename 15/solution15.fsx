// Dijkstra's single-source shortest path algorithm
type VertexAttributes<'a> =
    {
        dist: uint64 // an upper bound on distance from source
        pred: 'a option // predecesor that guarantees dist
    }
    static member Init =
        { dist = System.UInt64.MaxValue
          pred = None }
type VertexMap<'a when 'a:comparison> = Map<'a, VertexAttributes<'a>>

let dijkstra neighbourhood distance s =
    let getClosestNeighbour (sortedVertices: System.Collections.Generic.SortedSet<_>) =
        // let uName, uProp =
        //     Map.toList vertexMap
        //     |> List.minBy (fun (_ , prop) -> prop.dist)
        // Map.remove uName vertexMap, uName, uProp 
        let u = sortedVertices.Min
        ignore <| sortedVertices.Remove u
        u
    // let toVertexMap knownVertices names = 
    //     names 
    //     |> List.map (fun name -> if Map.containsKey name knownVertices then name,knownVertices.[name] else name,VertexAttributes<_>.Init)
    //     |> Map.ofList
    let relax (sortedVertices: System.Collections.Generic.SortedSet<_>) uName uProp vName =
        if vProp.dist > uProp.dist + distance uName vName then
            ignore <| sortedVertices.Remove (vName, vProp)
            ignore <| sortedVertices.Add (vName, {dist = uProp.dist + distance uName vName; pred = Some uName})
            // vertexMap
            // |> Map.add vName {dist = uProp.dist + distance uName vName; pred = Some uName}
        else
            () //vertexMap
    let rec step (exploredVertices: VertexMap<_>) (remainingVertices: System.Collections.Generic.SortedSet<_>) = //(remainingVertices: VertexMap<_>) =
        //if Map.isEmpty remainingVertices then
        if remainingVertices.Count = 0 then
            exploredVertices
        else
            let uName, uProp = getClosestNeighbour remainingVertices
            let newExploredVertices = Map.add uName uProp exploredVertices
            //let newRemainingVertices =
            neighbourhood uName
            |> List.filter (fun name -> not <| Map.containsKey name newExploredVertices)
            |> List.iter (relax remainingVertices uName uProp)
                // |> toVertexMap remainingVertices
                // |> Map.fold (relax uName uProp) remainingVertices
            step newExploredVertices remainingVertices
    let exploredVertices = Map.empty
    let remainingVertices = System.Collections.Generic.SortedSet<_>([s, {dist = 0UL; pred = None}])
        //Map.add s {dist = 0UL; pred = None} Map.empty
    step exploredVertices remainingVertices


// let cavern = array2D([
//     [1;1;6;3;7;5;1;7;4;2]
//     [1;3;8;1;3;7;3;6;7;2]
//     [2;1;3;6;5;1;1;3;2;8]
//     [3;6;9;4;9;3;1;5;6;9]
//     [7;4;6;3;4;1;7;1;1;1]
//     [1;3;1;9;1;2;8;1;3;7]
//     [1;3;5;9;9;1;2;4;2;1]
//     [3;1;2;5;4;2;1;6;3;9]
//     [1;2;9;3;1;3;8;5;2;1]
//     [2;3;1;1;9;4;4;5;8;1]
// ])
let cavern =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> System.UInt32.Parse))
    |> array2D

let enumarateNeighbours table x y =
    [
        if x > 0 then Some (x - 1, y) else None
        if x < Array2D.length1 table - 1 then Some (x + 1, y) else None
        if y > 0 then Some (x, y - 1) else None
        if y < Array2D.length2 table - 1 then Some (x, y + 1) else None
    ]
    |> List.choose id

let neighbourhood (x,y) = enumarateNeighbours cavern x y
let distance _ (x,y) = uint64 cavern[x,y]

let rec printPathTo (exploredVertices: VertexMap<_>) vertex =
    printfn "Vertex %A Total dist %i" vertex exploredVertices.[vertex].dist 
    match exploredVertices.[vertex].pred with
    | None -> ()
    | Some v -> printPathTo exploredVertices v

dijkstra neighbourhood distance (0,0)
// |> printPathTo <| (Array2D.length1 cavern - 1,Array2D.length2 cavern - 1)
|> Map.find (Array2D.length1 cavern - 1,Array2D.length2 cavern - 1)
|> printfn "Part A. %A"

let cavern5 =
    let incLimit x = 
        if x <= 9u then x
        else x - 9u
    let res = Array2D.create (5 * Array2D.length1 cavern) (5 * Array2D.length2 cavern) 0u
    for i in 0..4 do
        for j in 0..4 do
            cavern |> Array2D.iteri (fun x y value -> res[(i+1)*x, (j+1)*y] <- incLimit(cavern[x,y] + uint i + uint j) ) 
    res

let neighbourhood5 (x,y) = enumarateNeighbours cavern5 x y
let distance5 _ (x,y) = uint64 cavern5[x,y]
dijkstra neighbourhood5 distance5 (0,0)
// |> printPathTo <| (Array2D.length1 cavern - 1,Array2D.length2 cavern - 1)
|> Map.find (Array2D.length1 cavern5 - 1,Array2D.length2 cavern5 - 1)
|> printfn "Part B. %A"
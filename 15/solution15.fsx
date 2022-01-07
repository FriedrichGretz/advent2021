//#r "nuget: OptimizedPriorityQueue, 5.1.0"
//open Priority_Queue
#r "nuget: C5, 3.0.0-rc"

// Dijkstra's single-source shortest path algorithm
type Vertex<'a> =
    {
        value: 'a
        dist: uint64 // an upper bound on distance from source
        pred: 'a option // predecesor that guarantees dist
    }
type VertexMap<'a> = System.Collections.Generic.Dictionary<'a, Vertex<'a>>
type VertexCompare<'a> () =
    interface System.Collections.Generic.IComparer<Vertex<'a>> with
        member __.Compare (x, y) =
            if x.dist < y.dist then -1
            elif x.dist = y.dist then 0
            else 1

let dijkstra (neighbourhood: 'a -> 'a list) (distance: 'a -> 'a -> uint64) (initialVertexName: 'a) =
    let remainingVertices = C5.IntervalHeap<_>(VertexCompare<_>())
    let remainingHandles = System.Collections.Generic.Dictionary<_,C5.IPriorityQueueHandle<_>>()
    let exploredVertices = VertexMap<_>()
    
    let relax currentNode (neighbourHandle: C5.IPriorityQueueHandle<_>) =
        let mutable neighbour = Unchecked.defaultof<_>
        let ret = remainingVertices.Find(neighbourHandle, &neighbour)
        assert ret
        if neighbour.dist > currentNode.dist + distance currentNode.value neighbour.value then
            do ignore <| remainingVertices.Replace(neighbourHandle, {neighbour with dist = currentNode.dist + distance currentNode.value neighbour.value; pred = Some currentNode.value})
        else
            ()
    let rec step () =
        if remainingVertices.Count = 0 then
            exploredVertices
        else
            let vertex = remainingVertices.DeleteMin() // select closest vertex
            do ignore <| remainingHandles.Remove vertex.value // invalidate handle
            do ignore <| exploredVertices.Add(vertex.value, vertex) // add it to the completely explored vertices
            let neighbours = neighbourhood vertex.value // determine neighbours of this vertex and update their distances
            let neighbourHandles =
                neighbours
                |> List.choose (fun a -> 
                    if exploredVertices.ContainsKey a then 
                        None 
                    elif remainingHandles.ContainsKey a then
                        Some remainingHandles.[a]
                    else
                        let mutable handle = Unchecked.defaultof<_>
                        do ignore <| remainingVertices.Add(&handle,{value=a; dist = System.UInt64.MaxValue; pred = None})
                        remainingHandles.Add(a, handle) 
                        Some handle
                    )
            neighbourHandles |> List.iter (relax vertex)
            step ()
    
    let mutable handle = Unchecked.defaultof<_>
    do ignore <| remainingVertices.Add(&handle,{value=initialVertexName; dist = 0UL; pred = None})
    remainingHandles.Add(initialVertexName, handle)
    step ()


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
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> System.Int32.Parse))
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

let resultMap = dijkstra neighbourhood distance (0,0)
//printPathTo resultMap (Array2D.length1 cavern - 1, Array2D.length2 cavern - 1)
resultMap.[Array2D.length1 cavern - 1,Array2D.length2 cavern - 1].dist
|> printfn "Part A. Distance %i"

let cavern5 =
    let incLimit x = 
        if x <= 9 then x
        else x - 9
    let res = Array2D.create (5 * Array2D.length1 cavern) (5 * Array2D.length2 cavern) 0
    for i in 0..4 do
        for j in 0..4 do
            cavern |> Array2D.iteri (fun x y value -> res[(i * Array2D.length1 cavern)+x, (j * Array2D.length2 cavern)+y] <- incLimit(value + i + j) ) 
    res

//printfn "%A" cavern5

let neighbourhood5 (x,y) = enumarateNeighbours cavern5 x y
let distance5 _ (x,y) = uint64 cavern5[x,y]
let resultMap5 = dijkstra neighbourhood5 distance5 (0,0)
// printPathTo resultMap (Array2D.length1 cavern5 - 1, Array2D.length2 cavern5 - 1)
resultMap5.[Array2D.length1 cavern5 - 1,Array2D.length2 cavern5 - 1].dist
|> printfn "Part B. Distance %i"
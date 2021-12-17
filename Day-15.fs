module aoc_2021.Day15
open Util
open ArrayUtil
let day = "15"

type Point = int*int
type Entry = int*(Point list)
let shortestPath (arr:int[,]) =
    let dims = dims arr
    let (x1, y1) = (0, 0)
    let (x2, y2) = (dims.width-1, dims.height-1)
    let initial = (x2, y2)
    let maxValue = System.Int32.MaxValue
    let unvisited = System.Collections.Generic.PriorityQueue<Point, int>()
    for x in 0..dims.width-1 do
        for y in 0..dims.height-1 do
            unvisited.Enqueue((x, y), if (x, y) = initial then 0 else maxValue)
    let dists = Array2D.create dims.width dims.height maxValue
    let visited = Array2D.zeroCreate dims.width dims.height
    dists.[x2, y2] <- 0
    let rec loop current =
        let cx, cy = current
        let isUnvisited (px, py) = visited.[px, py] = 0
        let tentDist (x, y) = dists.[x, y]
        let neighbours = neighbours4 |> List.map (fun (dx, dy) -> (cx+dx, cy+dy)) |> List.filter (filterDim dims)
        let unvisitedNeighbours = neighbours |> List.filter isUnvisited
        let currentEdge = arr.[cx, cy]
        let currentDist = tentDist current
        for nb in unvisitedNeighbours do
            let tent = tentDist nb
            let nx, ny = nb
            let proposed = currentEdge + currentDist
            if proposed < tent then
                unvisited.Enqueue(nb, proposed)
                dists.[nx, ny] <- proposed

        if (cx, cy) = (x1, y1) then
            dists.[cx, cy]
        else
            visited.[cx, cy] <- 1
            let mutable next = unvisited.Dequeue()
            while not (isUnvisited next) do
                next <- unvisited.Dequeue()
            loop next

    let dist = loop initial
    dist

let calcA = shortestPath
let calcB arr =
    let dims = dims arr
    let w, h = dims.width, dims.height
    let initializer x y =
        let X, x = x/w, x%w
        let Y, y = y/h, y%h
        let v = arr.[x, y] + X + Y
        if v <= 9 then v else v%9
    let arr55 = Array2D.init (5*w) (5*h) initializer
    shortestPath arr55

let ex1 = splitA """
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
"""

let t, r = test32 day, test32 $"{day}R"
let read = readArray >> Array2D.map (fun c -> int c - int '0')
let tests =
    //printfn "todo"
    let input = ex1 |> read
    t (calcA input) 40
    t (calcB input) 315
    //t (calcA template rules) 1588
    //t (calcB template rules) 2188189693529L

let result =
    //printfn "todo"
    let input = readFile day |> read
    r (calcA input) 595
    r (calcB input) 2914

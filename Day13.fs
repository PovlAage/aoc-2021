module Day13
open Util
open ArrayUtil

let day = "13"
type Fold =
    | X of int
    | Y of int
let parseLine1 line =
    let s1, s2 = split2 "," line
    (int s1, int s2)

let parseLine2 line =
    let s1, s2 = split2 "=" line
    let n = int s2
    match s1.[s1.Length-1] with
    | 'x' -> X n
    | 'y' -> Y n
    | _ as c -> failwithf $"Cannot parse axis {c}"
let read lines =
    let chunks = splitOnBlankLines lines
    let lines1, lines2 = chunks.[0], chunks.[1]
    lines1 |> List.map parseLine1, lines2 |> List.map parseLine2
let apply1 f (x, y) =
    let reflect z0 z =
        z0 - (abs (z-z0))
    match f with
    | X x0 -> (reflect x0 x, y)
    | Y y0 -> (x, reflect y0 y)
let apply points f =
    let result = points |> List.map (apply1 f) |> List.distinct
    result
let calcA points folds =
    List.length (apply points (List.head folds))
let calcB points folds =
    folds |> List.fold apply points

let ex1 = splitKeepBlanks """
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"""
let toArray points =
    let width = (points |> List.map fst |> List.max) + 1
    let height = (points |> List.map snd |> List.max) + 1
    let arr = Array2D.zeroCreate width height
    for (x, y) in points do
        arr.[x, y] <- 1
    arr

let dumpPoints = toArray >> (dump (fun i -> if i = 0 then '.' else '#'))
let t, r = test32 day, test32 $"{day}R"

let tests _ =
    //printfn "todo"
    let points, folds = read ex1
    t (calcA points folds) 17
   // let input1 = ex1 |> List.map parseLine |> read
    // t (calcA input1) 10
    // t (calcB input1) 36

let result =
    //printfn "todo"
    let points, folds = readAndParseChunked day read
    r (calcA points folds) 850
    dumpPoints (calcB points folds)
    //r (calcA input) 3230
    //r (calcB input) 83475

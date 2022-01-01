module Day09
open Util
open ArrayUtil

let day = "09"

type Board = int[,]

let neighbours (x, y) =
    neighbours4 |> List.map (fun (dx, dy) -> (x+dx, y+dy))

let isLocalMin (board:Board) (x, y) =
    let entry = board.[x, y]
    neighbours (x, y) |> Seq.forall (fun (x, y) -> entry < board.[x, y])

let getLocalMins (board:Board) =
    board |> xyspadded |> Seq.filter (isLocalMin board) |> List.ofSeq

let getBasin (board:Board) (x, y) =
    let rec loop basin boundary =
        match boundary with
        | [] -> basin
        | head :: rest ->
            let existing = Set.union basin (Set.ofList rest)
            let nbs = neighbours head |> List.filter (fun (x, y) -> board.[x, y] <> 9 && not (existing.Contains (x, y)))
            loop (Set.add head basin) (nbs @ rest)
    loop (Set.empty) [(x, y)]

let calcA (board:Board) =
    let riskLevel (x, y) = 1+board.[x, y]
    (getLocalMins board) |> List.map riskLevel |> List.sum

let calcB board =
    let localMins = getLocalMins board
    localMins |> List.map (getBasin board >> Set.count) |> List.sortDescending |> List.take 3 |> List.reduce (*)

let parse = readArray >> (Array2D.map char2int) >> padArray 9

let ex = splitA """
2199943210
3987894921
9856789892
8767896789
9899965678
"""

let t, r = test32 day, test32 $"{day}R"

let tests _ =
//    printfn "todo"
    let input = ex |> parse
    t (calcA input) 15
    t (calcB input) 1134

let result =
    //printfn "todo"
    let input = readFile day |> parse
    r (calcA input) 588
    r (calcB input) 964712

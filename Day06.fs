module Day06
open Util

let day = "06"

let parseLine (line:string) =
    let initial = line.Split(",") |> Array.map int |> List.ofArray |> List.countBy id |> List.map (fun (x, y) -> (x, int64 y)) |> Map.ofList
    [for i in 0..8 do if initial.ContainsKey i then initial.[i] else 0L]

let rec sim steps school =
    let next (prev:int64 list) = function
    | 8 -> prev.[0]
    | 6 -> prev.[0] + prev.[7]
    | _ as i -> prev.[i+1]

    match steps with
    | 0 -> List.sum school
    | n -> sim (steps - 1) [for i in 0..8 do (next school i)]

let t = test64 day

let calcA steps lines = sim steps lines

let tests _ =
    let lines = "3,4,3,1,2" |> parseLine
    t (calcA 18 lines) 26L
    t (calcA 80 lines) 5934L
    t (calcA 256 lines) 26984457539L

let result =
    let lines = readAndParseSingle day parseLine
    t (calcA 80 lines) 352872L
    t (calcA 256 lines) 1604361182149L

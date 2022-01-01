module Day07
open Util

let day = "07"

let parseLine (line:string) =
    line.Split(",") |> Array.map int |> List.ofArray |> List.countBy id |> Map.ofList

let totalCost c m endpos =
    m |> Map.toSeq |> Seq.map (fun (pos, n) -> n*c(abs(pos-endpos))) |> Seq.sum

let costA = id

let costB dist = (dist+1)*dist/2

let calc cost m =
    let min = m |> Map.toSeq |> Seq.map fst |> Seq.min
    let max = m |> Map.toSeq |> Seq.map fst |> Seq.max
    [min..max] |> List.map (totalCost cost m) |> List.min

let calcA = calc costA
let calcB = calc costB
let t = test32 day

let tests _ =
    let ex = "16,1,2,0,4,2,7,1,2,14" |> parseLine
    t (totalCost costA ex 2) 37
    t (totalCost costA ex 1) 41
    t (totalCost costA ex 3) 39
    t (totalCost costA ex 10) 71
    t (calcA ex) 37
    t (totalCost costB ex 5) 168
    t (totalCost costB ex 2) 206
    t (calcB ex) 168

let result =
    //printfn "todo"
    let input = readAndParseSingle day parseLine
    t (calcA input) 349769
    t (calcB input) 99540554

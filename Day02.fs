module Day02
open System.Data
open Util

let day = "02"

type Command =
    | Up of int64
    | Down of int64
    | Forward of int64

let parseLine (s:string) =
    match s.Split(" ") with
    | [| "up"; ls |] -> Up (int64 ls)
    | [| "down"; ls |] -> Down (int64 ls)
    | [| "forward"; ls |] -> Forward (int64 ls)
    | _ -> failwithf $"Could not parse command {s}"

let parse lines = lines |> List.map parseLine

let executeA (ph:int64, pd:int64) = function
    | Forward n -> (ph+n, pd)
    | Up n -> (ph, pd-n)
    | Down n -> (ph, pd+n)
let executeB (ph:int64, pd:int64, aim:int64) = function
    | Forward n -> (ph+n, pd+n*aim, aim)
    | Up n -> (ph, pd, aim-n)
    | Down n -> (ph, pd, aim+n)
let calcA program =
    let (h, d) = program |> List.fold executeA (0L, 0L)
    h*d
    
let calcB program =
    let (h, d, a) = program |> List.fold executeB (0L, 0L, 0L)
    h*d

let example = split """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""

let t = test64 day
let tests _ =
    let ex = parse example
    t (calcA ex) 150L
    t (calcB ex) 900L

let result =
    let read = readAndParseFile day parseLine 
    t (calcA read) 1524750L
    t (calcB read) 1592426537L

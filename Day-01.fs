module aoc_2021.Day01
open Util

let day = "01"

let parseLine (s:string) = int64 s
let parse lines = lines |> List.map parseLine

let calcA =
    List.pairwise >> List.filter (fun (x, y) -> x<y) >> List.length >> int64

let calcB =
    List.windowed 3 >> List.map (List.sum) >> calcA

let example = split """
199
200
208
210
200
207
240
269
260
263
"""

let t = test64 day
let tests =
    let ex = parse example
    t (calcA ex) 7L
    t (calcB ex) 5L

let result =
    let read = readAndParseFile day parseLine 
    t (calcA read) 1832L
    t (calcB read) 1858L

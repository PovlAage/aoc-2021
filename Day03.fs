module Day03
open Util

let day = "03"

type Binary = Binary of (int64 list)
    
let width (Binary b) = b.Length
let asInt (Binary s) =
    let bits = [for i = 0 to s.Length-1 do (i, s.[s.Length-1-i])]
    let bit2int (pos:int, b:int64) = (pow2 pos) * b
    bits |> List.map bit2int |> List.reduce (+) |> int64
   
let parseChar = function
    | '0' -> 0L
    | '1' -> 1L
    | _ as c -> failwithf $"Invalid char '{c}'"
let formatChar = function
    | 0L -> '0'
    | 1L -> '1'
    | _ as c -> failwithf $"Invalid char '{c}'"
let parseLine (s:string) =
    Binary (s.ToCharArray() |> Array.map parseChar |> List.ofArray)
    
let extractBit b (Binary bb) =
    bb.[b]
let mostCommon bit (input:Binary list) =
    let half = (float input.Length / 2.0)
    let occurences = input |> List.map (extractBit bit) |> List.reduce (+)
    if float occurences >= half then 1L else 0L
let parse lines = lines |> List.map parseLine
let format b =
    System.String([| for i=0 to (width b)-1 do formatChar (extractBit i b) |])

let gamma input =
    let rec loop acc (input:Binary list) =
        if width (input.Head) = 0 then
            acc
        else
            let b = mostCommon 0 input
            let stripped = input |> List.map (fun (Binary b) -> Binary (List.tail b))
            loop (acc @ [b]) stripped
    asInt (Binary (loop [] input))

let lifeSupportRating bitCriterium input =
    let rec loop bit (input:Binary list) =
        match input with
        | [] -> failwithf "Empty list"
        | [single] -> single
        | _ ->
            let b = mostCommon bit input
            let filtered = input |> List.filter (fun bb -> (extractBit bit bb = b) = bitCriterium)
            loop (bit + 1) filtered
    asInt (loop 0 input)

let oxygenRating = lifeSupportRating true
let co2scrubberRating = lifeSupportRating false

let epsilon width input =
    (pow2 width - 1L) ^^^ (gamma input)
    
let calcA width input =
    (gamma input) * (epsilon width input)

let calcB input =
    (oxygenRating input) * (co2scrubberRating input)

let example = split """
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
"""

let t = test64 day

let tests _ =
    t (pow2 0) 1L
    t (pow2 1) 2L
    t (pow2 2) 4L
    t (asInt (parseLine "01010")) 10L
    t (calcA 5 (parse example)) 198L
    t (oxygenRating (parse example)) 23L
    t (co2scrubberRating (parse example)) 10L
    t (calcB (parse example)) 230L

let result =
    let read = readAndParseFile day parseLine 
    t (calcA 12 read) 749376L
    t (calcB read) 2372923L

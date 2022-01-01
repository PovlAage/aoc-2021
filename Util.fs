module Util
open Checked
open System
open System.IO

let rec pow2 = function
    | 0 -> 1L
    | n when n > 0 -> 2L*(pow2 (n-1))
    | _ as n -> failwithf $"Invalid exponent {n}"

let testgeneric<'T when 'T:equality> context (actual:'T) (expected:'T) = 
    if expected = actual then
        printfn $"{context} OK:\t{expected}"
    else
        printfn $"{context} FAIL:\t{actual}!={expected}"

let teststring context (expected:string) (actual:string)     =
    if expected = actual then
        printfn $"{context} OK:\t{expected}"
    else
        printfn $"{context} FAIL:\t{actual}!={expected}"

let test32 context (expected:int) (actual:int)     =
    if expected = actual then
        printfn $"{context} OK:\t{expected}"
    else
        printfn $"{context} FAIL:\t{actual}!={expected}"

let test64 context (expected:int64) (actual:int64)     =
    if expected = actual then
        printfn $"{context} OK:\t{expected}"
    else
        printfn $"{context} FAIL:\t{actual}!={expected}"

let getPath filename =
    Path.Join("Input", filename)

let filename day = $"input-{day}.txt"

let readFile day = File.ReadAllLines(getPath (filename day))

let readAndParseSingle day parseLine =
    File.ReadAllText(getPath (filename day)).Trim() |> parseLine

let readAndParseFile day parseLine =
    (readFile day) |> Seq.ofArray |>
    Seq.filter (String.IsNullOrWhiteSpace >> not) |> Seq.map (fun s -> s.Trim()) |>
    Seq.map parseLine |> List.ofSeq

let readAndParseChunked day parse =
    let filename = $"input-{day}.txt"
    parse (File.ReadAllLines(getPath filename) |> Seq.ofArray |> Seq.map (fun s -> s.Trim()) |> List.ofSeq)

let split (multiLine:string) =
    let options = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
    multiLine.Split(Environment.NewLine, options) |> List.ofArray

let splitA (multiLine:string) =
    let options = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
    multiLine.Split(Environment.NewLine, options)

let splitKeepBlanks (multiLine:string) =
    let options = StringSplitOptions.TrimEntries
    multiLine.Split(Environment.NewLine, options) |> Seq.ofArray |> Seq.skipWhile (String.IsNullOrWhiteSpace) |>
    List.ofSeq  

let split2 (sep:string) (line:string) =
    match line.Split(sep) with
    | [| a; b|] -> a, b
    | _ as s -> failwithf $"Not {sep}-separated pair: {s}"

let splitOnBlankLines lines =
    let firstChunk lines =
        match lines |> List.tryFindIndex (fun line -> String.IsNullOrWhiteSpace(line)) with
        | None -> lines, []
        | Some n -> let lines, rest = List.splitAt n lines in lines, rest.Tail
    let rec loop acc lines =
        match lines with
        | [] -> acc
        | _ -> let chunk, rest = firstChunk lines in loop (acc @ [chunk]) rest
    let result = loop [] lines
    assert (result.Length > 1)
    result

let char2int (c:char) = int c - int '0'


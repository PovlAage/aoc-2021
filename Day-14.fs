module aoc_2021.Day14
open Util
let day = "14"

type Rule = Rule of (char*char)*char
let parseLineTemplate (line:string) = line.ToCharArray() |> List.ofArray
let parseLineRule line =
    let a, b = split2 " -> " line
    Rule ((a.[0], a.[1]), b.[0])
let read lines =
    let chunks = splitOnBlankLines lines
    parseLineTemplate (List.exactlyOne chunks.[0]), chunks.[1] |> List.map parseLineRule
let tryParse (aa, bb) (Rule ((a, b), c)) =
    if aa = a && bb = b then Some ([a; c], b) else None
let tryParse2 (aa, bb) (Rule ((a, b), c)) =
    if aa = a && bb = b then Some c else None
let tryTake1 s =
    let first = Seq.tryHead s
    if first = None then None else Some (first.Value, Seq.skip 1 s)
let tryTake2 s =
    match tryTake1 s with
    | None -> [], s
    | Some (first, rest) ->
        match tryTake1 rest with
        | None -> [first], rest
        | Some (second, rest) -> [first; second], rest

let process1step rules tokens =
    let rec loop result tokens =
        match tokens with
        | a :: b :: rest ->
            let matches = rules |> List.choose (tryParse (a, b))
            match matches with
            | [] -> loop (a :: result) (b :: rest)
            | [m] -> let parsed, putback = m in loop (List.rev parsed @ result) (putback::rest)
            | _ -> failwithf $"More than one rule matched {[a;b]}: {matches}"
        | _ -> List.rev (tokens @ result)
    loop [] tokens
let process1step2 step rules (tokens:char seq) =
    seq {
        let mutable i = 0L
        let mutable last = 'x'
        for (a, b) in Seq.pairwise tokens do
//            printfn $"Step {step} i={i}"
            let matches = rules |> List.choose (tryParse2 (a, b))
            match matches with
            | [] -> yield a
            | [c] -> yield a ; yield c
            | _ -> failwithf $"More than one rule matched {[a;b]}: {matches}"
            last <- b
            i <- i+1L
        yield last
    }

let processSteps steps (tokens:char list) rules =
    let folder = fun t _ -> (process1step rules t)
    List.fold folder tokens [1..steps]
let processSteps2 steps (tokens:char seq) rules =
    let folder = fun t step -> (process1step2 step rules t)
    Seq.fold folder tokens [1..steps]
type Freqmap = Map<char, int64>
let merge (map1:Freqmap) (map2:Freqmap) =
    let tryAdd (a:int64) (b:int64 option) =
        if b.IsNone then Some a else Some (a+b.Value)
    let folder (m:Freqmap) (c, cc) =
        m |> Map.change c (tryAdd cc)
    (Map.toSeq map2) |> Seq.fold folder map1
let mapMinus1 c = Map.ofList [(c, -1L)]
let map2 a b = if a = b then Map.ofList [(a, 2L)] else Map.ofList [(a, 1L); (b, 1L)]
let calcFreqPair rules n (a, b) =
    let lookup = System.Collections.Generic.Dictionary<(int*char*char), Freqmap>()
    let rec loop n (a, b) =
        let mutable v = Map.empty
        if lookup.TryGetValue((n, a, b), &v) then
            v
        else
            let matches = rules |> List.choose (tryParse2 (a, b))
            let v = match n, matches with
                    | 0, _ -> map2 a b
                    | _, [] -> map2 a b
                    | _, [c] -> merge (mapMinus1 c) (merge (loop (n-1) (a, c)) (loop (n-1) (c, b)))
                    | _, _ -> failwithf $"More than one rule matched {[a;b]}: {matches}"
            let _ = lookup.TryAdd((n, a, b), v)
            v
        
    loop n (a, b)

let calcFreqMap rules n template =
    let countWithExtras = List.pairwise template |> List.map (calcFreqPair rules n) |> List.reduce merge
    let templateExceptEnds = template |> List.skip 1 |> List.take (List.length template - 2)
    let extras = templateExceptEnds |> List.map (fun c -> mapMinus1 c) |> List.reduce merge
    merge countWithExtras extras
let calcA tokens rules =
    let tokens = processSteps 10 tokens rules
    let sortedCounts = tokens |> List.countBy id |> List.map (snd >> int64) |> List.sort
    let countFirst, countLast = List.head sortedCounts, List.last sortedCounts
    countLast - countFirst
let calcB tokens rules =
    let freqmap = calcFreqMap rules 40 tokens
    let sortedCounts = freqmap |> Map.toList |> List.map snd |> List.sort
    let countFirst, countLast = List.head sortedCounts, List.last sortedCounts
    countLast - countFirst

    // let byCountSorted = tokens |> Seq.countBy id |> List.ofSeq |> List.sortBy snd
    // let (first, countFirst), (last, countLast) = List.head byCountSorted, List.last byCountSorted
    // countLast - countFirst

let ex1 = splitKeepBlanks """
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"""

let t, r = test64 day, test64 $"{day}R"
let list2string = Array.ofList >> System.String
let dumpFreqmap (m:Freqmap) =
    for (c, cc) in (Map.toList m |> List.sortBy fst) do
        printfn $"{c}:{cc}"
    printfn ""
let tests =
    //printfn "todo"
    let template, rules = ex1 |> read
    t (calcA template rules) 1588
    t (calcB template rules) 2188189693529L

let result =
    //printfn "todo"
    let template, rules = readAndParseMulti day read
    r (calcA template rules) 2010
    r (calcB template rules) 2437698971143L

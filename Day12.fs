module Day12
open Util

let day = "12"

type Cave =
    | Start
    | End
    | Large of string
    | Small of string
    | ExtraSmall

type Cavemap = Map<Cave, Cave list>

type Segment = Cave * Cave

let paths (cavemap:Cavemap) visit2spent =
    let rec loop (path:Cave list) (visited:Cave Set) =
        let current = List.head path
        match current with
        | End -> [List.rev path]
        | _ ->
            let allowVisit pos =
                match pos with
                | Large _ -> Some (pos, visited)
                | Small _ when (visited.Contains pos) && not (visited.Contains ExtraSmall) -> Some (pos, visited.Add ExtraSmall)
                | _ when not (visited.Contains pos) -> Some (pos, visited.Add pos)
                | _ -> None
            let allowedConnections = cavemap.[current] |> List.choose allowVisit
            allowedConnections |> List.map (fun (pos, visited) -> loop (pos :: path) visited) |> List.concat
    let initialVisited = if visit2spent then Set.singleton ExtraSmall else Set.empty
    loop [Start] (initialVisited.Add Start)

let calcA input = List.length (paths input true)
let calcB input = List.length (paths input false)

let parseLine (line:string) =
    let parseCave = function
    | "start" -> Start
    | "end" -> End
    | c when c = c.ToLowerInvariant() -> Small c
    | c when c = c.ToUpperInvariant() -> Large c
    | _ as c -> failwithf $"Could not parse cave {c}"
    let a, b = split2 "-" line
    ((parseCave a), (parseCave b))
let read (segments:Segment list) =
    let sym = List.append segments (segments |> List.map (fun (x, y) -> (y, x)))
    sym |> List.groupBy fst |> List.map (fun (x, xys) -> (x, List.map snd xys)) |> Map.ofList

let ex1 = split """
start-A
start-b
A-c
A-b
b-d
A-end
b-end
"""

let ex2 = split """
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
"""

let ex3 = split """
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
"""

let t, r = test32 day, test32 $"{day}R"

let tests _ =
//    printfn "todo"
    let input1 = ex1 |> List.map parseLine |> read
    let input2 = ex2 |> List.map parseLine |> read
    let input3 = ex3 |> List.map parseLine |> read
    t (calcA input1) 10
    t (calcA input2) 19
    t (calcA input3) 226
    t (calcB input1) 36
    t (calcB input2) 103
    t (calcB input3) 3509

let result =
    //printfn "todo"
    let input = readAndParseFile day parseLine |> read
    r (calcA input) 3230
    r (calcB input) 83475

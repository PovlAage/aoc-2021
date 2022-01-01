module Day10
open Util

let day = "10"

let openers = ['('; '['; '{'; '<']
let closers = [')'; ']'; '}'; '>']
let open2close = List.zip openers closers |> Map.ofList
type Result =
    | Corrupt of char
    | Incomplete of char list

let checkLine line =
    let rec loop line stack =
        match line, stack with
        | [], [] -> failwithf "Unexpected: Complete"
        | [], _ -> Incomplete stack // incomplete
        | head :: restLine, _ when open2close.ContainsKey head -> loop restLine (open2close.[head] :: stack)
        | closer :: restLine, expected :: restStack when closer = expected -> loop restLine restStack
        | closer :: _, _ -> Corrupt closer
    loop line []

let parseLine (line:string) =
    line.ToCharArray() |> List.ofArray

let scoreMapA = List.zip [')'; ']'; '}'; '>'] [3; 57; 1197; 25137] |> Map.ofList
let scoreA = function
    | Corrupt c -> scoreMapA.[c]
    | _ -> 0
let scoreMapB = List.zip [')'; ']'; '}'; '>'] [1L; 2L; 3L; 4L] |> Map.ofList
let scoreB = function
    | Incomplete stack when stack.Length = 0 -> failwithf "Empty stack incomplete"
    | Incomplete stack -> stack |> List.fold (fun s c -> s*5L+scoreMapB.[c]) 0L
    | _ -> 0L

let calcA lines =
    lines |> List.sumBy (checkLine >> scoreA)
let calcB lines =
    let lineScores = lines |> List.map (checkLine >> scoreB) |> List.filter (fun s -> s > 0)
    assert (lineScores.Length % 2 = 1)
    lineScores |> List.sort |> List.item (lineScores.Length/2)

let ex = split """
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
"""

let t, r = test64 day, test64 $"{day}R"

let tests _ =
    //printfn "todo"
    let input = ex |> List.map parseLine
    t (calcA input) 26397
    t (calcB input) 288957
    t (scoreB (Incomplete ("}}]])})]".ToCharArray() |> List.ofArray))) 288957
    t (scoreB (Incomplete (")}>]})".ToCharArray() |> List.ofArray))) 5566
    t (scoreB (Incomplete ("}}>}>))))".ToCharArray() |> List.ofArray))) 1480781
    t (scoreB (Incomplete ("]]}}]}]}>".ToCharArray() |> List.ofArray))) 995444
    t (scoreB (Incomplete ("])}>".ToCharArray() |> List.ofArray))) 294

let result =
    //printfn "todo"
    let input = readAndParseFile day parseLine
    r (calcA input) 442131
    r (calcB input) 3646451424L
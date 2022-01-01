module Day18
open Util
let day = "18"

type Token =
    | Left
    | Right
    | Num of int
    | Comma

let tokenize (line:string) =
    let rec loop acc p =
        if p = line.Length then
            acc
        else
            let p, token =
                match line[p] with
                | '[' -> (p+1), Left
                | ']' -> (p+1), Right
                | ',' -> (p+1), Comma
                | d when '0' <= d && d <= '9' ->
                                    let nondigits = [|'[';']';','|]
                                    let numlen = line[p..].IndexOfAny(nondigits)-1
                                    (p+numlen+1), Num (int(line[p..(p+numlen)]))
                | _ as c -> failwithf $"Bad token {c}"
            loop (token :: acc) p
    let tokensrev = loop [] 0
    List.rev tokensrev

let token2string = function
    | Num n -> string(n)
    | Left -> "["
    | Right -> "]"
    | Comma -> ","

let reduce1 (tokens:Token list) =
    let isRegular = function
        | Num _ -> true
        | _ -> false
    let forceRegular p =
        match tokens[p] with
        | Num r -> r
        | _ as t -> failwithf $"Expected regular at {p} but is {t}"
    let rec loopExplode nest p =
        if p = tokens.Length then
            None
        else
            match tokens[p] with
            | Left when nest >= 4 ->
                // explode
                let x, y = forceRegular (p+1), forceRegular (p+3)
                let leftPart =
                    let orig = List.take p tokens
                    match orig |> List.tryFindIndexBack isRegular with
                    | Some i -> orig |> List.updateAt i (Num (x + forceRegular i))
                    | None -> orig
                let rightPart =
                    let orig = List.skip (p+5) tokens
                    match orig |> List.tryFindIndex isRegular with
                    | Some i -> orig |> List.updateAt i (Num (y + forceRegular (i+p+5)))
                    | None -> orig
                Some (List.concat [leftPart; [Num 0] ; rightPart])
            | Left -> loopExplode (nest+1) (p+1)
            | Right -> loopExplode (nest-1) (p+1)
            | _ -> loopExplode nest (p+1)
    let rec loopSplit p =
        if p = tokens.Length then
            None
        else
            match tokens[p] with
            | Num r when r >= 10 ->
                // split
                let leftPart = List.take p tokens
                let rightPart = List.skip (p+1) tokens
                Some (List.concat [leftPart; [Left; Num (r/2); Comma; Num (r-r/2); Right] ; rightPart])
            | _ -> loopSplit (p+1)
    loopExplode 0 0 |> Option.orElse (loopSplit 0)

let reduce tokens =
    let rec loop tokens =
        match reduce1 tokens with
        | Some tokens ->
            loop tokens
        | None -> tokens
    loop tokens

type Number =
    | Regular of int
    | Compound of Number * Number

let parse (tokens:Token list) =
    let eat token pos = if tokens[pos] = token then pos+1 else failwithf $"Expected {token} at {pos}"
    let parseComma pos =
        if tokens[pos] = Comma then Some (pos+1) else None
    let rec parseNum pos =
        match tokens[pos] with
        | Num n -> Regular n, pos+1
        | Left ->
            let pos = eat Left pos
            let n1, pos = parseNum pos
            let pos = eat Comma pos
            let n2, pos = parseNum pos
            let pos = eat Right pos
            Compound (n1, n2), pos
        | _ as token -> failwith $"Unexpected token {token} at {pos}"
    fst (parseNum 0)

let parseLine = tokenize >> parse
let format2tokens n =
    let rec f = function
        | Regular n -> [Num n]
        | Compound (n1, n2) -> [Left] @ (f n1) @ [Comma] @ (f n2) @ [Right]
    f n
let format n =
    (format2tokens n) |> List.map token2string |> String.concat ""

let addAndReduce n1 n2 =
    Compound (n1, n2) |> format2tokens |> reduce |> parse
let rec magnitude = function
    | Regular n -> n
    | Compound (n1, n2) -> 3*(magnitude n1) + 2*(magnitude n2)
let calcA nums =
    nums |> List.reduce addAndReduce |> magnitude
let calcB nums =
    let indexed = nums |> List.indexed
    let differentPairs = List.allPairs indexed indexed |> List.filter (fun (p1, p2) -> fst p1 <> fst p2)
    differentPairs |> Seq.map (fun (p1, p2) -> (addAndReduce (snd p1) (snd p2)) |> magnitude) |> Seq.max

let exLarge = split """
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
"""
let exMagnitude = split """
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
"""
let t, r = test32 day, test32 $"{day}R"
let tests _ =
    // printfn "todo"
    let r i = Regular i
    let cr n1 n2 = Compound (Regular n1, Regular n2)
    let c n1 n2 = Compound (n1, n2)
    let c12 = cr 1 2
    testgeneric day (parseLine "[1,2]") c12
    testgeneric day (parseLine "[[1,2],3]") (c c12 (r 3))
    testgeneric day (parseLine "[9,[8,7]]") (c (r 9) (c (r 8) (r 7)))
    testgeneric day (parseLine "[[1,9],[8,5]]") (c (cr 1 9) (cr 8 5))

    let reduceEx = Compound (parseLine "[[[[4,3],4],4],[7,[[8,4],9]]]", parseLine ("[1,1]"))
    let reduceEx2 = (reduceEx |> format2tokens |> reduce1).Value |> parse
    testgeneric day $"{format reduceEx2}" "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"
    let reduceEx3 = (reduceEx2 |> format2tokens |> reduce1).Value |> parse
    testgeneric day $"{format reduceEx3}" "[[[[0,7],4],[15,[0,13]]],[1,1]]"
    let reduceEx4 = (reduceEx3 |> format2tokens |> reduce1).Value |> parse
    testgeneric day $"{format reduceEx4}" "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
    let reduceEx5 = (reduceEx4 |> format2tokens |> reduce1).Value |> parse
    testgeneric day $"{format reduceEx5}" "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
    let reduceEx6 = (reduceEx5 |> format2tokens |> reduce1).Value |> parse
    testgeneric day $"{format reduceEx6}" "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    
    let reduceAndFormat = List.map parseLine >> List.reduce addAndReduce >> format
    testgeneric day (reduceAndFormat ["[1,1]";"[2,2]";"[3,3]";"[4,4]"]) "[[[[1,1],[2,2]],[3,3]],[4,4]]"
    testgeneric day (reduceAndFormat ["[1,1]";"[2,2]";"[3,3]";"[4,4]";"[5,5]"]) "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    testgeneric day (reduceAndFormat ["[1,1]";"[2,2]";"[3,3]";"[4,4]";"[5,5]";"[6,6]"]) "[[[[5,0],[7,4]],[5,5]],[6,6]]"
    testgeneric day (addAndReduce (parseLine "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]") (parseLine "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]") |> format) "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    testgeneric day (reduceAndFormat exLarge) "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    t (magnitude (parseLine "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")) 3488
    t (calcA (exMagnitude |> List.map parseLine)) 4140
    t (calcB (exMagnitude |> List.map parseLine)) 3993
let result =
    // printfn "todo"
    let input = readAndParseFile day parseLine
    r (calcA input) 3411
    r (calcB input) 4680
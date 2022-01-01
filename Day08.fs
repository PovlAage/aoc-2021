module Day08
open Util

let day = "08"

type SegmentCount = SegmentCount of int

type DigitInfo = {
    digit: int
    chars: string
    pattern: int
    count: SegmentCount
}

type Segment = char
type Permutation = Permutation of Map<Segment,Segment>
type PPermutation = PPermutation of Map<Segment,Segment>
type Line = List<string> * List<string>

let combinePerm (PPermutation p1) (PPermutation p2) =
    let keys p = Set.ofSeq (Map.keys p)
    assert Set.isEmpty (Set.intersect (keys p1) (keys p2))
    PPermutation (Map.ofSeq (Seq.append (Map.toSeq p1) (Map.toSeq p2)))
let complete (PPermutation p) =
    assert (p.Count = 7)
    Permutation p

let digitsRaw = [
    (0, "abcefg", 0b1110111, 6);
    (1, "cf", 0b0010010, 2);
    (2, "acdeg", 0b1011101, 5);
    (3, "acdfg", 0b1011011, 5);
    (4, "bcdf", 0b0111010, 4);
    (5, "abdfg", 0b1101011, 5);
    (6, "abdefg", 0b1101111, 6);
    (7, "acf", 0b1010010, 3);
    (9, "abcdefg", 0b1111011, 6);
    (8, "abcdfg", 0b1111111, 7);
]
let digits = digitsRaw |> List.map (fun (d, cs, p, count) ->
    {digit=d; chars=cs; pattern=p; count=SegmentCount count})
let validDigits = digits |> List.map (fun d -> d.chars) |> Set.ofList
let validBits = digits |> List.map (fun d -> d.pattern) |> Set.ofList
let bits (s:string) =
    let bit c = pow2 (int('g')-int(c)) |> int
    s.ToCharArray() |> Array.map bit |> Array.reduce (|||)
let lookupDigit s =
    let digit = digits |> List.filter (fun d -> d.pattern = bits s) |> List.exactlyOne
    digit.digit

let permute (Permutation p) digit =
    digit |> String.map (fun c -> p.[c])

let isValidDigit p s =
    let decoded = permute p s |> bits
    let result = validBits.Contains decoded
    result

let isValidPerm p digits =
    digits |> List.forall (isValidDigit p)

let parseLine (line:string) : Line =
    match line.Split(" | ") with
    | [| uniques; code |] ->
        let uniques = uniques.Split() |> List.ofArray
        let code = code.Split() |> List.ofArray
        uniques, code
    | _ -> failwithf $"Could not split line at |: {line}"

let segCount d = digits |> List.find (fun di -> di.digit = d) |> fun di -> di.count

let calcA lines =
    let segments1478 = Set.ofList [1; 4; 7; 8] |> Set.map segCount
    let scoreLine code =
        let result = code |> List.map (String.length >> SegmentCount) |> List.filter segments1478.Contains |> List.length
        result
    lines |> List.sumBy (snd >> scoreLine)

let decodeLine (line:Line) =
    let uniques, code = line
    let uniquesSegCount = uniques |> Seq.map (String.length >> SegmentCount)
    let index d = uniquesSegCount |> Seq.indexed |> Seq.filter (fun (i, sc) -> sc = segCount d) |> Seq.map fst |> Seq.exactlyOne
    let i1 = index 1
    let i4 = index 4
    let i7 = index 7
    let i8 = index 8

    let pperm (coded:string) (correct:string) =
        assert (coded.Length = correct.Length)
        let m = Array.zip (coded.ToCharArray()) (correct.ToCharArray()) |> Map.ofArray
        PPermutation m

    let stringDiff (s1:string) (s2:string) =
        let toSet (s:string) = s.ToCharArray() |> Set.ofArray
        Set.difference (toSet s1) (toSet s2) |> Set.toArray |> System.String

    let cf = uniques.[i1]
    let a = stringDiff (uniques.[i7]) cf
    let bd = stringDiff (uniques.[i4]) cf
    let eg = stringDiff (uniques.[i8]) (cf + a + bd)
    let p1s = [pperm cf "cf"; pperm cf "fc"]
    let p7 = [pperm a "a"]
    let p4 = [pperm bd "bd"; pperm bd "db"]
    let p8 = [pperm eg "eg"; pperm eg "ge"]
    let combs = [for i in 0..1 do for j in 0..1 do for k in 0..1 do [p1s.[i]; p7.[0]; p4.[j]; p8.[k]]]
    let complete = combs |> List.map (List.reduce combinePerm >> complete)
    let isValidPerm p =
        uniques |> List.forall (isValidDigit p)
    let validPerms = complete |> Seq.filter isValidPerm
    let onlyPerm = validPerms |> Seq.exactlyOne
    let resultDigits = code |> List.map (permute onlyPerm) |> List.map (lookupDigit >> string)
    let result = resultDigits |> List.reduce (+) |> int
    result

let calcB lines =
    lines |> List.map decodeLine |> List.reduce (+)

let t = test32 day

let ex = split """
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"""

let tests _ =
//    printfn "todo"
    let lines = ex |> List.map parseLine
    t (calcA lines) 26
    t (calcB lines) 61229

let result =
    //printfn "todo"
    let input = readAndParseFile day parseLine
    t (calcA input) 272
    t (calcB input) 1007675

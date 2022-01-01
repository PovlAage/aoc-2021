module Day20
open Util
open ArrayUtil
let day = "20"

let char2bit = function
    | '.' -> 0
    | '#' -> 1
    | _ as c -> failwithf $"Unexpected char {c}"

let bit2int (pos:int, b:char) = if b = '#' then (pow2 pos |> int) else 0

type Image = char[,]
type Algo = char[]
let parseAlgo algoLines =
    let algoString = String.concat "" algoLines
    algoString.ToCharArray()
let parseImage = Array.ofList >> readArray >> padArray '.' >> padArray '.'
let window = [for dy in [1..-1..-1] do for dx in [1..-1..-1] do (dx, dy)]
let windowAt (arr:Image) x y =
    let bits = window |> List.map (fun (dx, dy) -> arr[x+dx, y+dy]) |> List.indexed |> List.map bit2int |> List.sum
    bits
let step s (arr:Image) (algo:Algo) : Image =
    let pad evenodd =
        if algo[0] = '#' then
            if evenodd % 2 = 1 then '.' else '#'
        else
            '.'
    let arr = padArray (pad s) arr
    let dims = dims arr
    let isBorder x y =
        x=0 || x=dims.width-1 || y=0 || y=dims.width-1
    let dest = Array2D.init dims.width dims.height (fun x y -> if isBorder x y then (pad (s+1)) else algo.[windowAt arr x y])
    dest
let steps count arr algo =
    let rec loop s arr =
        if s = count+1 then
            arr
        else
            loop (s+1) (step s arr algo)
    loop 1 arr
let calc count arr algo =
    let arr = steps count arr algo
    let dims = dims arr
    seq { for x in 0..dims.width-1 do for y in 0..dims.height-1 do yield arr[x,y]} |> Seq.filter (fun c -> c='#') |> Seq.length
let calcA = calc 2
let calcB = calc 50

let parse lines =
    match splitOnBlankLines lines with
    | [algoLines; imageLines] -> parseAlgo algoLines, parseImage imageLines
    | _ -> failwithf $"Could not chunk"

let ex = splitKeepBlanks """
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
"""

let t, r = test32 day, test32 $"{day}R"
let tests _ =
    // printfn "todo"
    let algo, image = ex |> parse
    t (calcA image algo) 35
    t (calcB image algo) 3351
let result =
    // printfn "todo"
    let algo, image = readAndParseChunked day parse
    t (calcA image algo) 4968
    t (calcB image algo) 16793
    // let input = readAndParseChunked day (parse >> List.map generateTransforms)

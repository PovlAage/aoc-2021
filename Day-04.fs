module aoc_2021.Day04
open System
open Util

let day = "04"

let parseDrawLine (s:string) =
    s.Split(',') |> Array.map int |> List.ofArray

type Board = Board of int[,]
type BoardState = BoardState of int[,]

let parseBoard (ss:string list) =
    let board = Array2D.create 5 5 0
    for row = 0 to 4 do
        let entries = ss.[row].Split(' ', System.StringSplitOptions.TrimEntries ||| System.StringSplitOptions.RemoveEmptyEntries)
        for col = 0 to 4 do
            board.[row, col] <- entries.[col] |> int
    Board board

let markboard (Board b) (BoardState s) n =
    for row = 0 to 4 do for col = 0 to 4 do if b.[row, col] = n then s.[row, col] <- 1
    (Board b, BoardState s)
let score (Board b) (BoardState s) =
    let rowsandcols = [for row in 0..4 do [for col in 0..4 do (row, col)]] @ [for col in 0..4 do [for row in 0..4 do (row, col)]]
    let hasComplete = rowsandcols |> List.exists (List.forall (fun (r, c) -> s.[r, c] <> 0))
    if hasComplete then
        List.sum [for row in 0..4 do for col in 0..4 do if s.[row, col] = 0 then b.[row, col] else 0]
    else
        0
        
let play numbers boards =
    let rec loop acc numbers bs =
        match numbers with
        | n :: tail ->
            let marked = bs |> List.map (fun (b, s) -> markboard b s n)
            let scored = marked |> List.map (fun (b, s) -> ((b, s), n * (score b s)))
            let complete, incomplete = scored |> List.partition (fun (_, s) -> s>0)
            loop ((complete |> List.map snd) @ acc) tail (incomplete |> List.map fst)
        | [] -> acc
    loop [] numbers (boards |> List.map (fun b -> (b, BoardState (Array2D.create 5 5 0))))

let parse lines =
    let chunks = splitOnBlankLines lines
    let numbers = parseDrawLine (chunks.[0].[0])
    let boards = chunks |> List.skip(1) |> List.map parseBoard
    numbers, boards
let calcA numbers boards =
    List.last (play numbers boards)
let calcB numbers boards =
    List.head (play numbers boards)

let example = splitKeepBlanks """
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""

let t = test32 day

let tests =
    let numbers, boards = parse example
    t (calcA numbers boards) 4512
    t (calcB numbers boards) 1924

let result =
    let numbers, boards = readAndParseMulti day parse
    t (calcA numbers boards) 51034
    t (calcB numbers boards) 5434

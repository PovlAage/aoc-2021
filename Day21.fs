module Day21
open Util
let day = "21"

type Player = {
    Pos:int
    Score:int
}
type State = {
    Turn:int
    Player1:Player
    Player2:Player
    Die:int array
    DiePos:int
    Rolls:int
}

type StateB = {
    Turn:int
    Player1:Player
    Player2:Player
}

let play (initial:State) =
    let rec loop (s:State) =
        let p, other = if s.Turn = 1 then s.Player1, s.Player2 else s.Player2, s.Player1
        let roll = [0..2] |> List.map (fun i -> s.Die[(s.DiePos+i)%100]) |> List.sum
        let newPos = (p.Pos + roll - 1) % 10 + 1
        let p = { p with Pos = newPos; Score = p.Score + newPos }
        let s = { s with
                    Turn = if s.Turn = 1 then 2 else 1
                    Player1 = if s.Turn = 1 then p else s.Player1
                    Player2 = if s.Turn = 2 then p else s.Player2
                    DiePos = (s.DiePos + 3) % 100
                    Rolls = s.Rolls + 3
                }
        if p.Score >= 1000 then
            other.Score*s.Rolls
        else
            loop s
    loop initial

let dist3 =
    Map.ofSeq (List.allPairs [1..3] (List.allPairs [1..3] [1..3]) |> List.countBy (fun (x, (y, z)) -> x+y+z) |> Seq.map (fun (k, v) -> (k, v)))

type Scores = Scores of int64*int64
type Result =
    | FinalScore of Scores
    | State of StateB
let p1 = FinalScore (Scores (1L, 0L))
let p2 = FinalScore (Scores (0L, 1L))
let addScores (Scores (x1, x2)) (Scores (y1, y2)) = Scores (x1+y1, x2+y2)
let scalarMult c (Scores (x1, x2)) = Scores (c*x1, c*x2)
let playB initial =
    let key (s:StateB) = (s.Turn, s.Player1.Pos, s.Player1.Score, s.Player2.Pos, s.Player2.Score)
    let lookup = System.Collections.Generic.Dictionary<(int*int*int*int*int), Scores>()
    let rec loop s =
        let turn = s.Turn
        let p, other = if turn = 1 then s.Player1, s.Player2 else s.Player2, s.Player1
        let newState roll =
            let newPos = (p.Pos + roll - 1) % 10 + 1
            let p = { p with Pos = newPos; Score = p.Score + newPos }
            let s = { s with
                        Turn = if turn = 1 then 2 else 1
                        Player1 = if turn = 1 then p else s.Player1
                        Player2 = if turn = 2 then p else s.Player2
                    }
            if p.Score >= 21 then
                if turn = 1 then p1 else p2
            else
                State s
        let rec score = function
        | FinalScore sc -> sc
        | State s ->
            let mutable v = Scores (0L, 0L)
            let k = key s
            if lookup.TryGetValue(k, &v) then
                v
            else
                let vv = score (loop s)
                lookup.TryAdd (k, vv) |> ignore
                vv
        let newStates = dist3.Keys |> Seq.map (fun roll -> let mult = dist3[roll] in (mult, newState roll))
        let scores = newStates |> Seq.map (fun (count, result) -> scalarMult count (score result))
        let added = scores |> Seq.reduce addScores
        FinalScore added
    loop initial

let initial pos1 pos2 =
    let initialp pos = { Pos=pos; Score=0 }
    {
        Turn=1
        Player1=initialp pos1
        Player2=initialp pos2
        Die=Array.init 100 (fun i -> i+1)
        DiePos=0
        Rolls=0
    }
let initialB pos1 pos2 =
    let initialp pos = { Pos=pos; Score=0 }
    {
        Turn=1
        Player1=initialp pos1
        Player2=initialp pos2
    }
let calcA pos1 pos2 =
    play (initial pos1 pos2)
let calcB pos1 pos2 =
    match playB (initialB pos1 pos2) with
    | FinalScore (Scores (p1, p2)) -> max p1 p2
    | _ as r -> failwithf $"Unexpected result {r}"

let t, r = test64 day, test64 $"{day}R"
let tests _ =
    // printfn "todo"
    t (calcA 4 8) 739785
    t (calcB 4 8) 444356092776315L
    // let algo, image = ex |> parse
    // t (calcA image algo) 35
    // t (calcB image algo) 3351
let result =
    // printfn "todo"
    r (calcA 5 8) 1067724
    r (calcB 5 8) 630947104784464L
    // let algo, image = readAndParseChunked day parse
    // t (calcA image algo) 4968
    // t (calcB image algo) 16793
    // // let input = readAndParseChunked day (parse >> List.map generateTransforms)

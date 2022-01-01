module Day17
open Util
let day = "17"

type Area = { left:int; right:int; top:int; bottom:int }
type Point = int*int
let contains (area:Area) (x, y) =
    area.left <= x && x <= area.right && area.bottom <= y && y <= area.top
let beyond (area:Area) (x, y) (dx, dy) =
    if abs dx > 0 || dy > 0 then
        false
    else
        if not (area.left <= x && x <= area.right) then
            true
        else
            y < area.bottom
type State = { p: Point; q: Point; maxy: int; intersected: bool }

let sim area initial =
    let step state =
        let { p=(x, y); q=(dx, dy); maxy=maxy } = state
        let nextstate = {
            p=(x+dx, y+dy)
            q=(sign dx * (max 0 ((abs dx)-1)), dy-1)
            maxy=max maxy (y+dy)
            intersected=contains area (x, y)
            }
        nextstate

    let inf = Seq.initInfinite id |> Seq.scan (fun s _ -> step s) initial
    let continueSim s = not (s.intersected || beyond area s.p s.q)
    let s = inf |> Seq.takeWhile continueSim |> Seq.last
    if contains area s.p then
        Some (initial.q, s.maxy)
    else
        None
    
let initial (dx, dy) = {p=(0, 0); q=(dx, dy); maxy=0; intersected=false }

let initials area =
    let dxs =
        if area.left <= 0 && area.right >= 0 then
            [0]
        else if area.left > 0 then
            [1..area.right]
        else
            [area.left..(-1)]
    let dys =
        if area.top > 0 then
            failwithf $"Unexpected"
        else
            [(area.bottom)..(-area.bottom)]
    [for dx in dxs do for dy in dys -> (dx, dy)]

let calcA area =
    (initials area) |> List.map initial |> List.choose (sim area) |> List.map snd |> List.max

let calcB area =
    (initials area) |> List.map initial |> List.choose (sim area) |> List.length

let ex = "target area: x=20..30, y=-10..-5"
let t, r = test32 day, test32 $"{day}R"
let tg = testgeneric day
let tests _ =
    // printfn "todo"
    let area = { left=20; right=30; bottom=(-10); top=(-5) }
    tg (contains area (28, -7)) true
    tg (contains area (19, -7)) false
    tg (contains area (31, -7)) false
    tg (contains area (25, -11)) false
    tg (contains area (19, -4)) false
    tg (Option.isSome(sim area (initial (7, 2)))) true
    tg (Option.isSome(sim area (initial (6, 3)))) true
    tg (Option.isSome(sim area (initial (9, 0)))) true
    tg (Option.isSome(sim area (initial (17, -4)))) false
    t ((sim area (initial (6, 9))).Value |> snd) 45
    t (calcA area) 45
    t (calcB area) 112

let result =
    // printfn "todo"
    let area = { left=143; right=177; bottom=(-106); top=(-71) }
    r (calcA area) 5565
    r (calcB area) 2118

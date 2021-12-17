module aoc_2021.Day11
open Util
open ArrayUtil

let day = "11"

let neighbours (x, y) =
    neighbours8 |> List.map (fun (dx, dy) -> (x+dx, y+dy))

let step arr =
    let dims = dims arr
    let inc x y =
        arr.[x, y] <- arr.[x, y] + 1
    let set x y v =
        arr.[x, y] <- v

    for (x, y) in xyspadded arr do
        inc x y

    let mutable flash = true
    let mutable countFlashes = 0
    while flash do
        flash <- false
        for (x, y) in xyspadded arr do
            if arr.[x, y] > 9 && arr.[x, y] < 100 then
                for (xx, yy) in neighbours (x, y) do
                    inc xx yy
                set x y 100
                flash <- true

    for (x, y) in xyspadded arr do
        if arr.[x, y] >= 100 then
            countFlashes <- countFlashes + 1
            set x y 0

    arr, countFlashes

let calcA steps arr =
    let rec loop count steps arr =
        if steps = 0 then
            count
        else
            let arr, flashesInStep = step arr
            loop (count+flashesInStep) (steps-1) arr
    loop 0 steps arr

let calcB arr =
    let dims = dims arr
    let octoCount = (dims.height-2)*(dims.width-2)
    let rec loop stepNumber arr =
        let arr, flashesInStep = step arr
        if octoCount = flashesInStep then
            stepNumber
        else
            loop (stepNumber+1) arr
    loop 1 arr

let parse = readArray >> (Array2D.map char2int) >> padArray System.Int32.MinValue

let ex1 = splitA """
11111
19991
19191
19991
11111
"""

let ex2 = splitA """
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"""

let t, r = test32 day, test32 $"{day}R"

let tests =
    //printfn "todo"
    let ex = ex1 |> parse
    let ex, _ = step ex
    let ex, _ = step ex
    let input = ex2 |> parse
    t (calcA 100 input) 1656
    let input = ex2 |> parse
    t (calcB input) 195

let result =
    //printfn "todo"
    let input = readFile day |> parse
    r (calcA 100 input) 1585
    let input = readFile day |> parse
    r (calcB input) 382
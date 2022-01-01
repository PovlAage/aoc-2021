module Day25
open Util
open ArrayUtil
let day = "25"

let step (arr:char[,]) =
    let {width=xdim; height=ydim} = dims arr
    let moveeast (a:char[,]) x y =
        match a[(x-1+xdim)%xdim, y], a[x, y], a[(x+1)%xdim, y] with
        | _, '>', '.' -> '.'
        | '>', '.', _ -> '>'
        | _, c, _ -> c
    let movesouth (a:char[,]) x y =
        match a[x, (y-1+ydim)%ydim], a[x, y], a[x, (y+1+ydim)%ydim] with
        | _, 'v', '.' -> '.'
        | 'v', '.', _ -> 'v'
        | _, c, _ -> c
    let arr2 = Array2D.init xdim ydim (moveeast arr)
    let arr3 = Array2D.init xdim ydim (movesouth arr2)
    arr3

let calcA arr =
    let rec loop count arr =
        let {width=xdim; height=ydim} = dims arr
        let arrnext = step arr
        let same = Seq.forall id (seq {for x in 0..xdim-1 do for y in 0..ydim-1 do arr[x, y] = arrnext[x, y]})
        if same then count else loop (count+1) arrnext
    loop 1 arr

let ex = splitA """
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
"""

let t, r = test32 day, test32 $"{day}R"
let tests _ =
    // printfn "todo"
    let input = ex |> readArray
    dump id input
    dump id (step input)
    t (calcA input) 58

let result =
    // printfn "todo"
    let input = readFile day |> readArray
    r (calcA input) 432

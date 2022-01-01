module ArrayUtil
let readArray (lines:string array) : char[,] =
    let width = (lines |> Array.head).Length
    let height = Array.length lines
    let arr = Array2D.zeroCreate width height
    for x in 0..width-1 do
        for y in 0..height-1 do
            arr.[x, y] <- lines.[y].Chars x
    arr

type Dims = {width:int; height:int}

let dims arr =
    {width=Array2D.length1 arr; height=Array2D.length2 arr}

let padArray value arr =
    let dims = dims arr
    let arrPadded = Array2D.create (dims.width+2) (dims.height+2) value
    Array2D.blit arr 0 0 arrPadded 1 1 dims.width dims.height
    arrPadded

let dumpImpl x1 x2 y1 y2 formatter (arr:'t[,]) =
    for y in y1..y2 do
        for x in x1..x2 do
            printf $"{formatter arr.[x, y]}"
        printfn ""

let dump formatter arr =
    let dims = dims arr
    dumpImpl 0 (dims.width-1) 0 (dims.height-1) formatter arr

let dumpPadded formatter arr =
    let dims = dims arr
    dumpImpl 1 (dims.width-2) 1 (dims.height-2) formatter arr

let xyspadded arr =
    let dims = dims arr
    seq {for x in 1..dims.width-1 do for y in 1..dims.height-1 do (x, y)}

let neighbours4 =
    [(-1, 0); (1, 0); (0, -1); (0, 1)]

let neighbours8 =
    [
        (-1, -1); (0, -1); (1, -1); 
        (-1, 0); (1, 0);
        (-1, 1); (0, 1); (1, 1)]

let filterDim dims (x, y) =
    0 <= x && x < dims.width && 0 <= y && y < dims.height

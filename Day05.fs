module Day05
open Util

let day = "05"

type Point = Point of (int*int)
type Line = Line of Point*Point

let parseLine (line:string) =
    let a, b = split2 " -> " line
    let parsePoint s = 
        let x, y = split2 "," s
        Point (int x, int y)
    Line (parsePoint a, parsePoint b)

let dir (Line (Point (x1, y1), Point (x2, y2))) =
    (sign (x2-x1), sign (y2-y1))

let to1 ncol (Point (x, y)) = y*ncol+x

let dir1 ncol line =
    dir line |> Point |> (to1 ncol)

let points1 ncol line =
    let (Line (p1, p2)) = line
    set [(to1 ncol p1)..(dir1 ncol line)..(to1 ncol p2)]
    
let extremeCoords minmax p1 p2 =
    let (Point (x1, y1)) = p1
    let (Point (x2, y2)) = p2
    Point (minmax x1 x2, minmax y1 y2)

let maxCoords = extremeCoords max

let dims lines =
    lines |> List.map (fun (Line (p1, p2)) -> maxCoords p1 p2) |> List.reduce maxCoords

let ncol lines = let (Point (x, y)) = dims lines in x+1

let isDiag (Line (Point (x1, y1), Point (x2, y2))) =
    x1 <> x2 && y1 <> y2

let calc lines =
    let ncol = ncol lines
    let folder state line =
        let existing, intersects = state
        let points = points1 ncol line
        (Set.union existing points, Set.union intersects (Set.intersect existing points))
    let _, intersects = lines |> List.fold folder (Set.empty<int>, Set.empty<int>)
    Set.count intersects

let calcA = List.filter (isDiag >> not) >> calc
let calcB = calc

let example = split """
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"""

let t = test32 day

let tests _=
    let lines = example |> List.map parseLine
    t (calcA lines) 5
    t (calcB lines) 12

let result =
//    printfn "todo"
    let lines = readAndParseFile day parseLine
    t (calcA lines) 5442
    t (calcB lines) 19571

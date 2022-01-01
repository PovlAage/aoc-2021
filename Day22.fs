module Day22
open Util

let day = "22"

type Interval =
    | Interval of int*int
    | Empty
type Cube =
    | Cube of Interval list
    | EmptyCube
let isNonEmptyi i = i <> Empty
let isNonEmptyCube c = c <> EmptyCube
let cube is =
    if List.length is <> 3 then
        failwithf $"Invalid cube {is}"
    elif List.contains Empty is then
        EmptyCube
    else
        Cube is

type Switch = { value:int; cube:Cube }
let interval x1 x2 =
    if x1 <= x2 then Interval (x1, x2) else Empty
let intersecti i1 i2 =
    match i1, i2 with
    | (Interval (x1, x2)), (Interval (y1, y2)) ->
        interval (max x1 y1) (min x2 y2)
    | Empty, _ -> Empty
    | _, Empty -> Empty
let intersect r1 r2 =
    match r1, r2 with
    | Cube r1, Cube r2 -> List.zip r1 r2 |> List.map (fun (i1, i2) -> intersecti i1 i2) |> cube
    | EmptyCube, _ -> EmptyCube
    | _, EmptyCube -> EmptyCube
let cuti s r =
    let v =
        match r, (intersecti r s) with
        | Empty, _ -> [Empty]
        | _, Empty -> [r]
        | r, s when r = s -> [r]
        | Interval (r1, r2), _ when r1 = r2 -> [r]
        | Interval (r1, r2), Interval (s1, s2) when r1 = s1 && s1 = s2 -> [interval r1 r1; interval (r1+1) r2]
        | Interval (r1, r2), Interval (s1, s2) when r1 = s1 && s2 < r2 -> [interval r1 s2; interval (s2+1) r2]
        | Interval (r1, r2), Interval (s1, s2) when r1 < s1 && s2 < r2 -> [interval r1 (s1-1); interval s1 s2; interval (s2+1) r2]
        | Interval (r1, r2), Interval (s1, s2) when r1 < s1 && s2 = r2 -> [interval r1 (s1-1); interval s1 r2]
        | _ -> failwithf $"Unexpected s:{s}, r:{r}"
    v |> List.filter isNonEmptyi
let verifyDisjoint cubes =
    ()
    // let cubesi = List.indexed cubes
    // let overlaps = List.allPairs cubesi cubesi |> List.filter (fun ((i1, c1), (i2, c2)) -> i1 <> i2 && Emptycube <> intersect c1 c2)
    // for o in overlaps do
    //     printfn $"Overlap {o}"
    // assert (0 = List.length overlaps)
let subtract s r =
    match s, r with
    | Cube ss, Cube rr when (intersect s r) = EmptyCube -> [r]
    | Cube ss, Cube rr ->
        let v = List.zip ss rr |> List.map (fun (ssi, rri) -> cuti ssi rri)
        let cubesBrutto = List.allPairs v[0] (List.allPairs v[1] v[2]) |> List.map (fun (x, (y, z)) -> cube [x;y;z])
        verifyDisjoint cubesBrutto
        cubesBrutto |> List.filter (fun t -> (intersect t r) <> EmptyCube && (intersect t s) = EmptyCube)
    | EmptyCube, r -> [r]
    | _, EmptyCube -> [EmptyCube]

let length = function
    | Interval (x1, x2) -> x2-x1+1
    | Empty -> 0
let volume = function
    | Cube is ->
        let v = is |> List.map (length >> int64) |> List.reduce (*)
        // printfn $"volume {is}={v}"
        v
    | EmptyCube -> 0L

let executeB (ss:Switch list) =
    let rec loop acc ss =
        match ss with
        | [] -> acc
        | {value=value; cube=cube} :: ss ->
            let subdivided = acc |> List.collect (subtract cube)
            verifyDisjoint subdivided
            let acc =
                if value = 1 then
                    cube :: subdivided
                else
                    subdivided
            verifyDisjoint subdivided
            loop acc ss
    let cubes = loop [] ss
    verifyDisjoint cubes
    cubes |> List.sumBy volume

let i50 = interval -50 50
let restrict50 s = { s with cube=intersect (Cube [i50;i50;i50]) s.cube }
let calcA = (List.map restrict50) >> executeB
let calcB = executeB

let parseInterval s =
    let a, b = split2 ".." s
    Interval (int(a), int(b))
let parseLine (line:string) =
    let ss = line.Split([|' ';'=';','|], System.StringSplitOptions.None)
    let parseAt i = parseInterval (ss[i])
    let parseValue =
        match ss[0] with
        | "on" -> 1
        | "off" -> 0
        | _ as v -> failwithf $"Bad value {v}"
    { value=parseValue; cube=cube [parseAt 2;parseAt 4;parseAt 6]}


let ex = split """
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
"""

let ex2 = split """
on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682
"""

let t, r = test64 day, test64 $"{day}R"
let tests _ =
    // printfn "todo"
    let input = ex |> List.map parseLine
    t (length Empty) 0
    t (length (Interval (10, 10))) 1
    t (length (Interval (10, 11))) 2
    t (volume EmptyCube) 0
    t (volume (cube [Interval (10, 10); Interval (10, 11); Interval (10, 12)])) 6
    let i28 = interval 2 8
    testgeneric day (cuti (interval 0 1) i28) [i28]
    testgeneric day (cuti (interval 0 3) i28) [interval 2 3; interval 4 8]
    testgeneric day (cuti (interval 0 9) i28) [i28]
    testgeneric day (cuti (interval 2 4) i28) [interval 2 4; interval 5 8]
    testgeneric day (cuti (interval 3 3) i28) [interval 2 2; interval 3 3; interval 4 8]
    testgeneric day (cuti (interval 3 4) i28) [interval 2 2; interval 3 4; interval 5 8]
    testgeneric day (cuti (interval 3 8) i28) [interval 2 2; interval 3 8]
    testgeneric day (cuti (interval 3 9) i28) [interval 2 2; interval 3 8]
    testgeneric day (cuti (interval 7 9) i28) [interval 2 6; interval 7 8]
    testgeneric day (cuti (interval 8 9) i28) [interval 2 7; interval 8 8]
    testgeneric day (cuti (interval 9 9) i28) [i28]
    let i01 = Interval (0, 1)
    let i0 = Interval (0, 0)
    let i00 = Interval (-1, 0)
    let i1 = Interval (1, 1)
    testgeneric day (subtract (cube [i1;i1;i1]) (cube [i0;i0;i0])) [cube [i0;i0;i0]]
    testgeneric day (subtract (cube [i01;i01;i01]) (cube [i01;i01;i01])) []
    testgeneric day (subtract (cube [i0;i01;i01]) (cube [i01;i01;i01])) [cube [i1;i01;i01]]
    testgeneric day (set(subtract (cube [i0;i01;i0]) (cube [i01;i01;i01]))) (set([cube [i0;i01;i1]; cube [i1;i01;i0]; cube [i1;i01;i1]]))
    t (subtract (cube [i0;i0;i0]) (cube [i01;i01;i01])).Length 7
    t (subtract (cube [i00;i0;i0]) (cube [i01;i01;i01])).Length 7
    // testgeneric day (set(subtract (cube [i00;i00;i00]) (cube [i01;i01;i01]))) (set([cube [i1;i01;i01]; cube [i01;i1;i01]; cube [i01;i01;i1]; cube [i01;i1;i1]; cube [i1;i01;i1]; cube [i1;i1;i01]; cube [i1;i1;i1]]))
    t (calcA (input |> List.map restrict50)) 39
    t (calcB (input |> List.map restrict50)) 39

let result =
    // printfn "todo"
    let input = readAndParseFile day parseLine
    t (calcB (input |> List.map restrict50)) 652209
    t (calcB input) 1217808640648260L

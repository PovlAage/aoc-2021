module Day19
open Util
let day = "19"

let mdims m = (Array2D.length1 m, Array2D.length2 m)
type Mat = int[,]
type RotId = RotId of int
let matmul x y =
    let xdims, ydims = (mdims x), (mdims y)
    assert (snd xdims = fst ydims)
    let d = snd xdims
    Array2D.init (fst xdims) (snd ydims) (fun i j -> List.sum [for k in 0..(d-1) -> x[i,k]*y[k,j] ])

let rots2d =
    let rot1 = Array2D.zeroCreate 2 2
    rot1[0, 1] <- 1
    rot1[1, 0] <- -1
    let rot2 = matmul rot1 rot1
    let rot3 = matmul rot2 rot1
    let rot4 = matmul rot3 rot1
    assert (rot4[0, 0] = 1 && rot4[1,0] = 0 && rot4[0,1] = 0 && rot4[1,1] = 1)
    [rot4;rot1;rot2;rot3]

let rots3d =
    let verify22 m = assert (mdims m = (2, 2))
    let block k m =
        verify22 m
        let excisek l = if l<k then l else l-1
        Array2D.init 3 3 (fun i j -> if i=k || j=k then (if i=j then 1 else 0) else m[excisek i, excisek j])
    let rotsx = rots2d |> List.map (block 0)
    let rotsy = rots2d |> List.map (block 1)
    let rotsz = rots2d |> List.map (block 2)
    let gross = List.allPairs rotsx (List.allPairs rotsy rotsz) |> List.map (fun (rx, (ry, rz)) -> matmul (matmul rx ry) rz)
    printfn $"gross count: {gross.Length}"
    let arr2list (arr:int[,]) =
        [for i in 0..2 do for j in 0..2 -> arr[i, j]]
    let distinct = gross |> List.distinctBy arr2list
    printfn $"distinct count: {distinct.Length}"
    distinct |> List.indexed |> List.map (fun (i, m) -> (RotId i, m))

let transpose (m:int[,]) =
    Array2D.init (Array2D.length2 m) (Array2D.length1 m) (fun i j -> m[j, i])

let printfnMat m =
    let dims = mdims m
    for i in 1..(fst dims) do
        for j in 1..(snd dims) do
            let v = m[i-1, j-1]
            let s = if sign v = -1 then " " else "  "
            printf $"{s}{v}"
        printfn ""
    
type Rot = Mat

type ScannerId = ScannerId of int
type Point = Point of int*int*int
type Symmetry = Symmetry of int*Point
type ScannerRaw = ScannerRaw of ScannerId*int[,]
type Translation = Translation of int*int*int
type ScannerData = ScannerData of ScannerId*List<RotId*Translation*Set<Point>>
let getTranslation (Point (x1, y1, z1)) (Point (x2, y2, z2)) =
    Translation (x2-x1, y2-y1, z2-z1)
let addTranslations (Translation (x1, y1, z1)) (Translation (x2, y2, z2)) = Translation (x1+x2, y1+y2, z1+z2)
let inverseTranslation (Translation (x1, y1, z1)) = Translation (-x1, -y1, -z1)
let zeroTranslation = Translation (0, 0, 0)
let getPoint k (points:int[,]) =
    Point (points[0, k], points[1, k], points[2, k])
let translate (Translation (tx, ty, tz)) (Point (px, py, pz)) = Point (px+tx, py+ty, pz+tz)
let centert (Point (x, y, z)) = Translation (-x, -y, -z)
let center (Point (x, y, z)) points =
    let vec = [| x; y; z |]
    points |> Array2D.mapi (fun i j v -> v-vec[i])

let transform points rot translation =
    let rotated = matmul rot points
    let centered = center translation rotated
    centered
let vecToSet points =
    let _, n = mdims points
    [0..(n-1)] |> List.map (fun k -> getPoint k points) |> Set.ofList

let allTransforms points =
    let _, n = mdims points
    let translations rotatedPoints = [0..(n-1)] |> List.map (fun k -> centert (getPoint k rotatedPoints))
    let rotated = rots3d |> List.map (fun (ri, rot) -> (ri, matmul rot points))
    let transformed = rotated |> List.collect (fun (ri, rotatedPoints) -> (translations rotatedPoints) |> List.map (fun t -> (ri, inverseTranslation t, rotatedPoints |> vecToSet |> Set.map (translate t))))
    transformed

let countCommonPoints points1 points2 = (Set.intersect points1 points2).Count
// let getMatches (ri1, tp1, points1) (ri2, tp2, points2)
let isOverlap (ScannerData (id1, list1)) (ScannerData (id2, list2)) =
//    let overlappers = List.allPairs list1 list2 |> List.filter (fun ((r1, t1, ps1), (r2, t2, ps2)) -> countCommonPoints ps1 ps2 >= 12)
    let overlappers = List.allPairs list1 list2 |> List.filter (fun ((r1, t1, ps1), (r2, t2, ps2)) -> countCommonPoints ps1 ps2 >= 12)
    match overlappers.Length with
    | 0 ->
        //printfn $"No overlaps {id1} {id2}"
        None
    | _ ->
        let o1, o2 = overlappers[0]
        let (r1, t1:Translation, ps1) = o1
        let (r2, t2:Translation, ps2) = o2
        // let translation = [t0; inverseTranslation (centert p1); getTranslation p1 p2] |> List.reduce addTranslations
        // printfn $"Overlap {id1} {id2}"
        Some (t1, id2, r2, t2, ps2)

let findOverlaps (allTransformed:ScannerData list) =
    let filter rotid (ScannerData (id, transformedList)) =
        ScannerData (id, transformedList |> List.filter (fun (r, _, _) -> r = rotid))
    let rec loop (alreadyChecked:Set<ScannerId*ScannerId>) (acc:(Translation*ScannerData) list) (accPoints:Set<Point>) (remaining:Set<ScannerData>) =
        // printfn $"{Set.count accPoints} beacons"
        // for b in accPoints do
        //     printfn $"{b}"
        if Set.isEmpty remaining then
            accPoints, (acc |> List.map fst)
        else
            let id (ScannerData (sid, _)) = sid
            let (i, t0, sd1, (t1, sid2, r2, t2, ps2)) = Seq.allPairs acc remaining |> Seq.indexed |> Seq.filter (fun (i, ((t0, sd1), sd2)) -> not (Set.contains (id sd1, id sd2) alreadyChecked)) |> Seq.choose (fun (i, ((t0, sd1), sd2)) -> let iso = isOverlap sd1 sd2 in if iso.IsNone then None else Some (i, t0, sd1, iso.Value)) |> Seq.head
            let alreadyChecked = Set.union alreadyChecked (Seq.allPairs acc remaining |> Seq.take (i+1) |> Seq.map (fun ((t0, sd1), sd2) -> (id sd1, id sd2)) |> Set.ofSeq)

            let (ScannerData (sid1, _)) = sd1
            let sd2 = remaining |> Seq.filter (fun (ScannerData (id, _)) -> id = sid2) |> Seq.exactlyOne
            let t = [t0; t1; inverseTranslation t2] |> List.reduce addTranslations
            let acc = ((t, filter r2 sd2) :: acc)
            let translatedPoints = (ps2 |> Set.map (translate (addTranslations t0 t1)))
            // printfn $"accPoints#:{Set.count accPoints}, translatedPoints#:{Set.count translatedPoints}"
            let intersect = Set.intersect accPoints translatedPoints
            let accPoints = Set.union accPoints translatedPoints
            let remaining = remaining |> Set.remove sd2
            // printfn $"Overlap {sid1} and {sid2}, t={t}, intersect#={Set.count intersect}, intersect={intersect}"
            loop alreadyChecked acc accPoints remaining
            // accPoints
    
    let initial = filter (RotId 0) allTransformed[0]
    let initialPoints = let (ScannerData (_, l)) = allTransformed[0] in let _, t0, ps = l[0] in (t0, ps)
    let initialTranslation = fst initialPoints
    loop Set.empty [(zeroTranslation, initial)] (snd initialPoints |> Set.map (translate initialTranslation)) (Set.ofList(allTransformed |> List.skip 1))

let calcAB transformed =
    let accPoints, accCenters = findOverlaps transformed
    let mandist ((Translation (x1, y1, z1)), (Translation (x2, y2, z2))) =
        abs (x2-x1) + abs (y2-y1) + abs (z2-z1)
    let calcA = Set.count accPoints
    let calcB = List.allPairs accCenters accCenters |> List.map mandist |> List.max
    calcA, calcB

let parseHeader (s:string) = ScannerId (int(fst (s.Substring(12) |> split2 " ")))
let parsePoint (s:string) =
    let vec3 = s.Split(",", System.StringSplitOptions.None) |> Array.map int
    assert (vec3.Length = 3)
    vec3
let generateTransforms (ScannerRaw (scanner, pointsArr)) =
    ScannerData (scanner, allTransforms pointsArr)

let parseChunk (chunk:string list) =
    let scanner = parseHeader chunk[0]
    let pointsList = chunk[1..] |> List.map parsePoint
    let pointsArr = Array2D.init 3 (chunk.Length-1) (fun i j -> pointsList[j][i])
    ScannerRaw (scanner, pointsArr)

let parse lines =
    splitOnBlankLines lines |> List.map parseChunk
let ex = splitKeepBlanks """
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
"""

let t, r = test32 day, test32 $"{day}R"
let tests =
    // printfn "todo"
    let input = ex |> parse
    let transformed = input |> List.map generateTransforms

    let accPoints, accCenters = findOverlaps transformed
    printfn $"{Set.count accPoints} beacons"
    let a, b = calcAB transformed
    t a 79
    t b 3621
let result =
//    printfn "todo"
    let input = readAndParseChunked day (parse >> List.map generateTransforms)
    let a, b = calcAB input
    r a 381
    r b 12201

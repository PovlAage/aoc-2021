[<EntryPoint>]
let Main(args) =
    let day = if args.Length > 0 then Some (int(args[0])) else None
    let runday d =
        match day with
        | Some day when d = day -> true
        | Some day -> false
        | None -> true
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let printElapsed before =
        printfn $"{int((sw.Elapsed-before).TotalMilliseconds)} ms"
    let time (f:Lazy<_>) =
        let before = sw.Elapsed
        f.Force()
        let after = sw.Elapsed
        let elapsedms = int((after-before).TotalMilliseconds)
        if elapsedms > 100 then
            printfn $"{elapsedms} ms"

    let beforeall = sw.Elapsed
    if runday 1 then
        let before = sw.Elapsed
        // Day01.tests
        time (lazy Day01.result)
        printElapsed before
    if runday 2 then
        let before = sw.Elapsed
        // Day02.tests
        time (lazy Day02.result)
        printElapsed before
    if runday 3 then
        let before = sw.Elapsed
        // Day03.tests
        time (lazy Day03.result)
        printElapsed before
    if runday 4 then
        let before = sw.Elapsed
        // Day04.tests
        time (lazy Day04.result)
        printElapsed before
    if runday 5 then
        let before = sw.Elapsed
        // Day05.tests
        time (lazy Day05.result)
        printElapsed before
    if runday 6 then
        let before = sw.Elapsed
        // Day06.tests
        time (lazy Day06.result)
        printElapsed before
    if runday 7 then
        let before = sw.Elapsed
        // Day07.tests
        time (lazy Day07.result)
        printElapsed before
    if runday 8 then
        let before = sw.Elapsed
        // Day08.tests
        time (lazy Day08.result)
        printElapsed before
    if runday 9 then
        let before = sw.Elapsed
        // Day09.tests
        time (lazy Day09.result)
        printElapsed before
    if runday 10 then
        let before = sw.Elapsed
        // Day10.tests
        time (lazy Day10.result)
        printElapsed before
    if runday 11 then
        let before = sw.Elapsed
        // Day11.tests
        time (lazy Day11.result)
        printElapsed before
    if runday 12 then
        let before = sw.Elapsed
        // Day12.tests
        time (lazy Day12.result)
        printElapsed before
    if runday 13 then
        let before = sw.Elapsed
        // Day13.tests
        time (lazy Day13.result)
        printElapsed before
    if runday 14 then
        let before = sw.Elapsed
        // Day14.tests
        time (lazy Day14.result)
        printElapsed before
    if runday 15 then
        let before = sw.Elapsed
        // Day15.tests
        time (lazy Day15.result)
        printElapsed before
    if runday 16 then
        let before = sw.Elapsed
        // Day16.tests
        time (lazy Day16.result)
        printElapsed before
    if runday 17 then
        let before = sw.Elapsed
        // Day17.tests
        time (lazy Day17.result)
        printElapsed before
    if runday 18 then
        let before = sw.Elapsed
        //time (lazy (Day18.tests 0))
        time (lazy Day18.result)
        printElapsed before
    if runday 19 then
        let before = sw.Elapsed
        // Day19.tests
        time (lazy Day19.result)
        printElapsed before
    if runday 20 then
        let before = sw.Elapsed
        // Day20.tests
        Day20.result
        printElapsed before
    if runday 21 then
        let before = sw.Elapsed
        // Day21.tests
        Day21.result
        printElapsed before
    if runday 22 then
        let before = sw.Elapsed
        // Day22.tests
        Day22.result
        printElapsed before
    if runday 23 then
        let before = sw.Elapsed
        // Day23.tests
        Day23.result
        printElapsed before
    if runday 24 then
        let before = sw.Elapsed
        Day24.tests 0
        Day24.result
        printElapsed before
    if runday 25 then
        let before = sw.Elapsed
        // Day25.tests
        Day25.result
        printElapsed before
    let afterall = sw.Elapsed
    let elapsedms = int((afterall-beforeall).TotalMilliseconds)
    printfn $"Total elapsed: {elapsedms} ms"

    0
let sw = System.Diagnostics.Stopwatch.StartNew()
let time (f:Lazy<_>) =
    let before = sw.Elapsed
    f.Force()
    let after = sw.Elapsed
    let elapsedms = int((after-before).TotalMilliseconds)
    if elapsedms > 100 then
        printfn $"{elapsedms} ms"

// aoc_2021.Day01.tests
time (lazy aoc_2021.Day01.result)
// aoc_2021.Day02.tests
time (lazy aoc_2021.Day02.result)
// aoc_2021.Day03.tests
time (lazy aoc_2021.Day03.result)
// aoc_2021.Day04.tests
time (lazy aoc_2021.Day04.result)
//aoc_2021.Day05.tests
time (lazy aoc_2021.Day05.result)
// aoc_2021.Day06.tests
time (lazy aoc_2021.Day06.result)
// aoc_2021.Day07.tests
time (lazy aoc_2021.Day07.result)
// aoc_2021.Day08.tests
time (lazy aoc_2021.Day08.result)
// aoc_2021.Day09.tests
time (lazy aoc_2021.Day09.result)
// aoc_2021.Day10.tests
time (lazy aoc_2021.Day10.result)
// aoc_2021.Day11.tests
time (lazy aoc_2021.Day11.result)
// aoc_2021.Day12.tests
time (lazy aoc_2021.Day12.result)
// aoc_2021.Day13.tests
time (lazy aoc_2021.Day13.result)
// aoc_2021.Day14.tests
time (lazy aoc_2021.Day14.result)
// aoc_2021.Day15.tests
time (lazy aoc_2021.Day15.result)
// aoc_2021.Day16.tests
time (lazy aoc_2021.Day16.result)
// aoc_2021.Day17.tests
time (lazy aoc_2021.Day17.result)

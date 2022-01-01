module Day24
open Util
let day = "24"

type Variable = W | X | Y | Z

type Lval = Lvar of Variable
type Rval =
    | Rvar of Variable
    | Number of int64

type Instruction =
    | INP of Lval
    | ADD of Lval*Rval
    | MUL of Lval*Rval
    | DIV of Lval*Rval
    | MOD of Lval*Rval
    | EQL of Lval*Rval

type Program = Instruction list
type Input = int list
type Memory = { W:int64; X:int64; Y:int64; Z:int64 }

let readmem mem = function
    | W -> mem.W
    | X -> mem.X
    | Y -> mem.Y
    | Z -> mem.Z

let writemem mem v = function
    | W -> {mem with W=v}
    | X -> {mem with X=v}
    | Y -> {mem with Y=v}
    | Z -> {mem with Z=v}

let executeold (program:Program) (input:Input) =
    let rec loop (mem:Memory) ppos ipos =
        let readmem = readmem mem
        let writemem = writemem mem
        if ppos = program.Length then
            mem
        else
            let readl = function
                | Lvar v -> readmem v
            let readr = function
                | Rvar v -> readmem v
                | Number n -> n
            let instruction = program[ppos]
            let lval, v, ipos =
                match instruction with
                | INP l -> l, int64 input[ipos], ipos+1
                | ADD (l, r) -> l, (readl l) + (readr r), ipos
                | MUL (l, r) -> l, (readl l) * (readr r), ipos
                | DIV (l, r) -> l, (readl l) / (readr r), ipos
                | MOD (l, r) -> l, (readl l) % (readr r), ipos
                | EQL (l, r) -> l, (if (readl l) = (readr r) then 1L else 0L), ipos
            let writel (Lvar v) (n:int64) = writemem n v
            let mem = writel lval v
            loop mem (ppos+1) ipos
    loop {W=0; X=0; Y=0; Z=0} 0 0

let pprint mn = mn |> List.rev |> List.map string |> (String.concat "") |> int64

// let monad (a:bool) b c w z =
//     // inp w
//     // mul x 0
//     // add x z
//     // mod x 26
//     // add x b
//     // eql x w
//     // eql x 0
//     // let x = not (w = z%26+b)

//     // div z a
//     // let z = if a then z/26 else z
    
//     // mul y 0
//     // add y 25
//     // mul y x
//     // add y 1
//     // mul z y
    
//     // let z = z*(1+25*x)

//     // mul y 0
//     // add y w
//     // add y c
//     // mul y x
//     // add z y
    
//     // let z = z+x*(w+c)
//     // (if a then z else z*26) + (if (w = z%26+b) then 0 else w+c)

//     // last: a=26, b=-14, c=3
//     // x = not (w = z%26-14)
//     // 0 = z
//     // <=>
//     // 0 = if x=1 then z+w+3 else z/a
//     // if x=1 then z+w+3=0 else z=0
//     // enten z=-w-3
//     // eller w=-14 og z=0
//     (z/(if a then 26 else 1)+w+c)*(if not (w = z%26+b) then 26 else 1)
// let monads =
//     [
//         monad 1 13 14
//         monad 1 12 8
//         monad 1 11 5
//         monad 26 0 4
//         monad 1 15 10
//         monad 26 -13 13
//         monad 1 10 16
//         monad 26 -9 5
//         monad 1 11 6
//         monad 1 13 13
//         monad 26 -14 6
//         monad 26 -3 7
//         monad 26 -2 13
//         monad 26 -14 3
//     ]

let runsection (program:Program) (start:int) (stop:int) (mem:Memory) =
    let rec loop (mem:Memory) ppos =
        let readmem = readmem mem
        let writemem = writemem mem
        if ppos = stop then
            mem
        else
            let readl = function
                | Lvar v -> readmem v
            let readr = function
                | Rvar v -> readmem v
                | Number n -> n
            let writel (Lvar v) (n:int64) = writemem n v
            let instruction = program[ppos]
            let lval, v =
                match instruction with
                | INP _ -> failwithf $"INP should not appear here"
                | ADD (l, r) -> l, (readl l) + (readr r)
                | MUL (l, r) -> l, (readl l) * (readr r)
                | DIV (l, r) -> l, (readl l) / (readr r)
                | MOD (l, r) -> l, (readl l) % (readr r)
                | EQL (l, r) -> l, (if (readl l) = (readr r) then 1L else 0L)
            let mem = writel lval v
            loop mem (ppos+1)

    loop mem start

let searchLast (program:Program) =
    let isINP = function
        | INP _ -> true
        | _ -> false
    let lastINP = 1 + (program |> List.findIndexBack isINP)
    printfn $"Executing from {lastINP}"
    let digits = [1..9]
    let memories = seq {
        for w in digits do for x in digits do for y in digits do for z in digits do {W=w;X=x;Y=y;Z=z}
    }
    printfn $"Trying {List.length (List.ofSeq memories)}"
    let isAccepted memory = memory.Z = 0
    let acceptedMemories = memories |> Seq.filter (runsection program (lastINP+1) (List.length program) >> isAccepted)
    acceptedMemories

let search (program:Program) digits =
    let lookup = System.Collections.Generic.Dictionary<(Memory*int), bool>()
    let rec loop (acc:int64 list) (mem:Memory) ppos =
        let mutable v = false
        if lookup.TryGetValue((mem, ppos), &v) then
            None
        else
            let readmem = readmem mem
            let writemem = writemem mem
            if ppos = program.Length then
                if readmem Z = 0 then
                    Some acc
                else
                    None
            else
                let readl = function
                    | Lvar v -> readmem v
                let readr = function
                    | Rvar v -> readmem v
                    | Number n -> n
                let writel (Lvar v) (n:int64) = writemem n v
                let instruction = program[ppos]
                let vv =
                    match instruction with
                    | INP l ->
                        digits |> Seq.map int64 |> Seq.map (fun d -> (d, writel l d)) |> Seq.choose (fun (d, mem) -> loop (d::acc) mem (ppos+1)) |> Seq.tryHead
                    | _ ->
                        let lval, v =
                            match instruction with
                            | INP _ -> failwithf $"INP should not appear here"
                            | ADD (l, r) -> l, (readl l) + (readr r)
                            | MUL (l, r) -> l, (readl l) * (readr r)
                            | DIV (l, r) -> l, (readl l) / (readr r)
                            | MOD (l, r) -> l, (readl l) % (readr r)
                            | EQL (l, r) -> l, (if (readl l) = (readr r) then 1L else 0L)
                        let mem = writel lval v
                        loop acc mem (ppos+1)
                if vv.IsNone && acc.Length >= 6 then
                    if List.take 6 acc = [5;5;5;5;5;5] then
                        printfn $"Caching {pprint acc}"
                    lookup.TryAdd((mem, ppos), false) |> ignore
                    vv
                else
                    vv

    loop [] {W=0; X=0; Y=0; Z=0} 0


let isAcceptedMN program (i, mn) =
    let mem = executeold program mn
    if i % 100 = 0 then
        let v = mem.Z
        printfn $"{i}, {pprint mn}: {v}"
    mem.Z = 0

let calcA program =
    (search program [9..-1..1]).Value |> pprint

let calcB program =
    (search program [1..9]).Value |> pprint

let parseline (line:string) =
    let parsevar = function
        | "w" -> W
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | _ as n -> failwithf $"Bad variable name {n}"
        
    let parselval s = Lvar s
    let parserval (s:string) =
        let mutable i = 0L
        if System.Int64.TryParse(s, &i) then
            Number i
        else
            Rvar (parsevar s)
    let tokens = line.Split(' ')
    let instr, lval = tokens[0], parselval (parsevar tokens[1])
    if instr = "inp" then
        INP lval
    else
        let rval = parserval tokens[2]
        match instr, lval, rval with
        | "add", l, r -> ADD (l, r)
        | "mul", l, r -> MUL (l, r)
        | "div", l, r -> DIV (l, r)
        | "mod", l, r -> MOD (l, r)
        | "eql", l, r -> EQL (l, r)
        | _ -> failwithf $"Bad instruction {instr}"
let t, r = test64 day, test64 $"{day}R"
let tests _ =
    // printfn "todo"
    let ex1 = split "inp x\nmul x -1" |> List.map parseline
    printfn $"{ex1}"
    let mem = executeold ex1 [10]
    t mem.X -10L

let result =
    printfn "todo"
    let program = readAndParseFile day parseline
    let accepted = List.ofSeq (searchLast program)
    printfn $"Accepted: {accepted.Length}"
    for m in accepted do
        printfn $"{m}"
    // r (calcA program) 93499629698999L
    // r (calcB program) 11164118121471L

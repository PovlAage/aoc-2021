module aoc_2021.Day16
open Util
let day = "16"

let hexc2bin = function
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ as c -> failwithf $"Invalid hex {c}"

type Version = Version of int
type TypeId = TypeId of int
type Literal = Literal of int64
type Operation =
    | Operation of int

type Packet =
    | LiteralPacket of Version * Literal
    | OperatorPacket of Version * Operation * (Packet list)

let hexs2bin (s:string) =
    s.ToCharArray() |> Array.map hexc2bin |> String.concat ""
let bin2int (s:string) =
    let bit (s:string) k =
        match s.[s.Length-k-1] with
        | '0' -> 0L
        | '1' -> 1L
        | _ as c -> failwithf $"Binary {s} has invalid {k}th bit {c}"
    [0..s.Length-1] |> List.map (fun k -> (bit s k) * pow2 k) |> List.sum
let literal groups =
    groups |> List.indexed |> List.map (fun (k, g) -> (pow2 (4*k)) * g) |> List.sum |> Literal
let tryTake (s:string) n =
    if s.Length >= n then
        Some (s.Substring(0, n), s.Substring(n))
    else
        None
let parseVersion s =
    match tryTake s 3 with
    | Some (version, rest) -> Some (Version (bin2int version |> int), rest)
    | None -> None
let parseTypeId s =
    match tryTake s 3 with
    | Some (typeId, rest) -> Some (TypeId (bin2int typeId |> int), rest)
    | None -> None
let parseHeader s =
    match parseVersion s with
    | Some (version, rest) ->
        match parseTypeId rest with
        | Some (typeId, rest) -> Some (version, typeId, rest)
        | None -> None
    | None -> None
let parseLiteral s =
    let parseLiteralGroup (s:string) =
        if s.Length < 5 then
            None
        else
            Some ((s[0], bin2int s[1..4]), s[5..])
    let rec loop acc (s:string) =
        match parseLiteralGroup s with
        | Some (('1', g), rest) -> loop (g :: acc) rest
        | Some (('0', g), rest) -> literal (g :: acc), rest
        | _ -> failwithf $"Could not parse literal {s}"
    loop [] s

let parseOperation version operation (parsePacket:string->(Packet*string) Option) (s:string) =
    let subPackets, rest = 
        if s[0] = '0' then
            let length = s[1..15] |> bin2int |> int
            let subpackets = s[16..16+length-1]
            let rest0 = s[16+length..]
            let rec parsePacketsLoop acc s =
                match parsePacket s with
                | Some (p, rest) -> parsePacketsLoop (p :: acc) rest
                | None -> List.rev acc, rest0
            parsePacketsLoop [] subpackets
        else
            let number = s[1..11] |> bin2int |> int
            let rest = s[12..]
            let rec parsePacketsLoop n acc s =
                if n = 0 then
                    List.rev acc, s
                else
                    match parsePacket s with
                    | Some (p, rest) -> parsePacketsLoop (n-1) (p :: acc) rest
                    | None -> failwithf $"Expected subpacket @s={s}"
            parsePacketsLoop number [] rest
    Some (OperatorPacket (version, operation, subPackets), rest)

let rec parsePacket s =
    match parseHeader s with
    | Some (version, typeId, rest) ->
        match typeId with
        | TypeId 4 ->
            let literal, rest = parseLiteral rest
            Some (LiteralPacket (version, literal), rest)
        | TypeId opid ->
            parseOperation version (Operation opid) parsePacket rest
    | None -> None

let rec eval p =
    match p with
    | LiteralPacket (_, Literal v) -> v
    | OperatorPacket (_, Operation 0, ps) -> ps |> List.map eval |> List.sum
    | OperatorPacket (_, Operation 1, ps) -> ps |> List.map eval |> List.reduce (*)
    | OperatorPacket (_, Operation 2, ps) -> ps |> List.map eval |> List.min
    | OperatorPacket (_, Operation 3, ps) -> ps |> List.map eval |> List.max
    | OperatorPacket (_, Operation 5, [p1; p2]) -> if (eval p1) > (eval p2) then 1 else 0
    | OperatorPacket (_, Operation 6, [p1; p2]) -> if (eval p1) < (eval p2) then 1 else 0
    | OperatorPacket (_, Operation 7, [p1; p2]) -> if (eval p1) = (eval p2) then 1 else 0
    | _ -> failwithf $"Unexpected OperatorPacket {p}"

let calcA hex =
    let p = hex |> hexs2bin |> parsePacket
    let rec loop p =
        match p with
        | LiteralPacket (Version v, _) -> v
        | OperatorPacket (Version v, _, ps) -> v + (ps |> List.map loop |> List.sum)
    let p, rest = p.Value
    loop p

let calcB hex =
    let p = hex |> hexs2bin |> parsePacket
    let p, rest = p.Value
    eval p

let exLiteral = "D2FE28" |> hexs2bin
let exOperator0 = "38006F45291200" |> hexs2bin
let exOperator1 = "EE00D40C823060" |> hexs2bin
let t, r = test64 day, test64 $"{day}R"
let ts = teststring day
let tp expected actual =
    let context = day
    match actual with
    | Some (actual, rest) ->
        if expected = actual then
            printfn $"{context} OK:\t{expected}"
        else
            printfn $"{context} FAIL:\t{actual}!={expected}"
    | None -> printfn $"Did not parse: {actual}"

let read = hexs2bin
let tests =
    // printfn "todo"
    ts exLiteral "110100101111111000101000"
    ts exOperator0 "00111000000000000110111101000101001010010001001000000000"
    ts exOperator1 "11101110000000001101010000001100100000100011000001100000"
    t (bin2int "110") 6
    t (bin2int "011111100101") 2021
//    printfn $"Parsed: {parsePacket exLiteral}"
//    printfn $"Parsed: {parsePacket exOperator0}"
//    printfn $"Parsed: {parsePacket exOperator1}"
    let createLit v l = LiteralPacket (Version v, Literal l)
    let createOp v o (vls:(int*int64)list) =
        OperatorPacket (Version v, Operation o, vls |> List.map (fun (v, l) -> createLit v l))
    tp (createLit 6 2021L) (parsePacket exLiteral)
    tp (createOp 1 6 [(6, 10L); (2, 20L)]) (parsePacket exOperator0)
    tp (createOp 7 3 [(2, 1L); (4, 2L); (1, 3L)]) (parsePacket exOperator1)
    t (calcA "8A004A801A8002F478") 16
    t (calcA "620080001611562C8802118E34") 12
    t (calcA "C0015000016115A2E0802F182340") 23
    t (calcA "A0016C880162017C3686B18A3D4780") 31
    t (calcB "C200B40A82") 3
    t (calcB "04005AC33890") 54
    t (calcB "880086C3E88112") 7
    t (calcB "CE00C43D881120") 9
    t (calcB "D8005AC2A8F0") 1
    t (calcB "F600BC2D8F") 0
    t (calcB "9C005AC2F8F0") 0
    t (calcB "9C0141080250320F1802104A08") 1

let result =
    // printfn "todo"
    let input = readAndParseSingle day id
    r (calcA input) 947
    r (calcB input) 660797830937L

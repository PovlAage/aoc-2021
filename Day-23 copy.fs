module aoc_2021.Day23
open Util

let day = "23"

type Result = Done | NotDone
type Species = A | B | C | D
type Room =
    | Hallway of int
    | Room of Species*int
let speciespos = function
    | A -> 2
    | B -> 4
    | C -> 6
    | D -> 8
let roomnames = [A;B;C;D]
let room n1 i1 =
    if (not (List.contains n1 roomnames)) || i1 < 1 then failwithf $"Invalid room {n1} {i1}"
    Room (n1, i1)
let rooms depth = List.allPairs [A;B;C;D] [1..depth] |> List.map Room
type Connection = { source:Room; dest:Room; cost:int; path:Room Set }
type Amphi = { species:Species }
type State = { occupied:Map<Room, Species>; occupiedset:Set<Room>; correctbelow:Map<Species,int>}
type Move = Move of Species*Connection

let walk source dest =
    let rec loop acc p =
        if p = dest then
            acc
        else
            let nextpos =
                match p, dest with
                | Hallway h1, Hallway h2 -> Hallway (h1 + sign(h2-h1))
                | Hallway h1, Room (a, _) ->
                    let roompos = speciespos a
                    if roompos = h1 then
                        Room (a, 1)
                    else
                        Hallway (h1 + sign(roompos-h1))
                | Room (a, m), Room (b, n) when a = b && m<n -> Room (a, m+1)
                | Room (a, m), _ when m>1 -> Room (a, m-1)
                | Room (a, 1), _ -> Hallway (speciespos a)
                | _, _ -> failwithf $"Unexpected move {p}->{dest}"
            loop (nextpos :: acc) nextpos
    List.rev (loop [] source)

let connection (s,d) =
    let path = walk s d
    {source=s; dest=d; cost=path.Length; path=set(path)}

let reverse c =
    {c with dest=c.source; source=c.dest; path=c.path |> Set.remove c.dest |> Set.add c.source}

type ConnectionMap = Map<Room*Room, Connection>
let connections depth =
    let roomcells = rooms depth
    let h2r =
        let hc h =
            roomcells |> List.map (fun r -> (Hallway h, r))
        [0;1;3;5;7;9;10] |> List.collect hc |> List.map connection
    let r2h = h2r |> List.map reverse
    let r2r =
        let insameroom (rc1, rc2) =
            match rc1, rc2 with
            | Room (a, _), Room (b, _) when a = b -> true
            | _ -> false
        List.allPairs roomcells roomcells |> List.filter (insameroom >> not) |> List.map connection
    List.concat [h2r;r2h;r2r] |> List.map (fun c -> ((c.source, c.dest), c)) |> Map.ofSeq

let eval state =
    let athome (r:Room, s:Species) =
        match r with
        | Room (ss, _) when ss = s -> true
        | _ -> false
    if state.occupied |> Map.toSeq |> Seq.forall athome then
        Done
    else
        NotDone

let isHome (Move (s, c)) =
    match c.dest with
    | Room (ss, _) when ss = s -> true
    | _ -> false

let islegal (state:State) (Move (s, c)) =
    let correctbelow s = Map.find s state.correctbelow
    match c.source, c.dest with
    | Room (ss, i), _ when ss = s && i>correctbelow s -> false
    | _, Room (ss, i) when ss = s && i<correctbelow s -> false
    | _, Room (ss, _) when ss <> s -> false
    | _, _ -> Set.intersect c.path state.occupiedset |> Set.isEmpty

let enumeratemoves (connections:ConnectionMap) (state:State) =
    let isFrom p (c:Connection) = c.source = p
    let movesFor (r, s) =
        connections.Values |> Seq.filter (isFrom r) |> Seq.map (fun c -> Move (s, c))
    let moves = state.occupied |> Map.toSeq |> Seq.collect movesFor |> Seq.filter (islegal state)
    let home, nothome = moves |> Seq.toList |> List.partition isHome
    if not (List.isEmpty home) then List.take 1 home else nothome

let cost (Move (s, c)) =
    let mult = match s with
                | A -> 1
                | B -> 10
                | C -> 100
                | D -> 1000
    mult * c.cost

let executeMove (state:State) (m as Move (s, c)) =
    let changecorrect =
        match c.dest with
        | Room (s, _) -> Some s
        | _ -> None
    {
        state with
            occupied = state.occupied |> Map.remove c.source |> Map.add c.dest s
            occupiedset = state.occupiedset |> Set.remove c.source |> Set.add c.dest
            correctbelow =
                if changecorrect.IsSome then
                    state.correctbelow |> Map.change (changecorrect.Value) (Option.map (fun i -> i-1))
                else
                    state.correctbelow
    }

let maxcost = 1000000
let search connections initial =
    // todo use best to truncate search
    let lookup = System.Collections.Generic.Dictionary<State, int>()
    let rec loop best (state:State) =
        let mutable v = 0
        if lookup.TryGetValue(state, &v) then
            v
        else
            let vv = match eval state with
                        | Done ->  0
                        | NotDone ->
                            let moves = enumeratemoves connections state
                            if Seq.isEmpty moves then
                                maxcost
                            else
                                let folder bestlocal move =
                                    let score = cost move + loop bestlocal (executeMove state move)
                                    min bestlocal score
                                let bestlocal = moves |> Seq.fold folder maxcost
                                bestlocal
            lookup.TryAdd (state, vv) |> ignore
            vv
    loop maxcost initial

let calcA = search (connections 2)
let calcB = search (connections 4)

let state depth initialpositions correctbelow : State =
    let rooms = rooms depth
    {
        occupied=Map.ofList (List.zip rooms initialpositions)
        occupiedset=set(rooms)
        correctbelow=Map.ofList correctbelow
    }

let t, r = test32 day, test32 $"{day}R"
let tests _ =
    // printfn "todo"
    let findcon source dest = (connections 2) |> Map.find (source, dest)
    t (findcon (Room (C, 1)) (Hallway 3)).cost 4
    t (findcon (Room (B, 1)) (Room (C, 1))).cost 4
    t (findcon (Room (B, 2)) (Hallway 5)).cost 3
    t (findcon (Hallway 3) (Room (B, 2))).cost 3
    t (calcA (state 2 [B;A;C;D;B;C;D;A] [(A,1);(B,2);(C,1);(D,2)])) 12521
    t (calcB (state 4 [B;D;D;A;C;C;B;D;B;B;A;C;D;A;C;A] [(A,3);(B,4);(C,3);(D,4)])) 44169

let result =
    // printfn "todo"
    r (calcA (state 2 [D;C;B;A;A;D;C;B] [(A,2);(B,2);(C,2);(D,2)])) 15538
    r (calcB (state 4 [D;D;D;C;B;C;B;A;A;B;A;D;C;A;C;B] [(A,4);(B,4);(C,4);(D,4)])) 47258

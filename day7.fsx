
#r @"nuget: FSharpPlus, 1.2.5"

open FSharpPlus

let input = System.IO.File.ReadAllLines "7.txt"

let sources =
    [
        for line in input do
            let s, w = sscanf "%s -> %s" line
            yield w, s

    ] |> Map.ofList

let (|INPUT|_|) s = trySscanf "%s" s
let (|VALUE|_|) s = trySscanf "%u" s
let (|AND|_|) s = trySscanf "%s AND %s" s
let (|OR|_|) s = trySscanf "%s OR %s" s
let (|NOT|_|) s = trySscanf "NOT %s" s
let (|LSHIFT|_|) s = trySscanf "%s LSHIFT %d" s
let (|RSHIFT|_|) s = trySscanf "%s RSHIFT %d" s

let values = System.Collections.Generic.Dictionary<string, uint16>()
let rec run wire =
    printfn $"{wire}"
    match wire with
    | VALUE v -> uint16 v
    | _ ->
        match values |> Dict.tryGetValue wire with
        | Some v -> v
        | _ ->
            let computed = 
                match sources[wire] with
                | NOT w -> ~~~ (run w)
                | AND(w1, w2) -> (run w1) &&& (run w2)
                | OR(w1, w2) -> (run w1) ||| (run w2)
                | LSHIFT(w, n) -> (run w) <<< n
                | RSHIFT(w, n) -> (run w) >>> n
                | VALUE v -> uint16 v
                | INPUT w -> run w
                | _ -> failwith "wrong input"
            values.Add(wire, computed)
            computed

run "a"
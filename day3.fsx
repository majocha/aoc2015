#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = System.IO.File.ReadAllText "3.txt" |> Seq.toList

let move (x, y) c =
    match c with
    | '>' -> x + 1, y
    | '<' -> x - 1, y
    | '^' -> x, y - 1
    | 'v' -> x, y + 1
    | _ -> failwith "wrong data"

input |> List.scan move (0, 0) |> List.distinct |> List.length

let chooseEven i c =
    if i % 2 = 0 then Some c else None

let chooseOdd i c =
    if i % 2 <> 0 then Some c else None

let one = input |> List.choosei chooseEven |> List.scan move (0, 0)
let two = input |> List.choosei chooseOdd |> List.scan move (0, 0)

one @ two |> List.distinct |> List.length

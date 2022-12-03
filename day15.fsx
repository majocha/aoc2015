#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = 
    System.IO.File.ReadAllLines "15.txt" 
    |> map (sscanf "%s: capacity %d, durability %d, flavor %d, texture %d, calories %d")
    |> map (fun (_, c, d, f, t, cal) -> [|c; d; f; t; cal|] )

let score (a: int array array) (proportions: int array) =
    let totals = [
        for i in 0..3 do
        [| for j in 0..3 -> a[j][i] * proportions[j] |] |> sum
    ]
    if totals |> exists (fun p -> p < 0) then 0
    else totals |> List.reduce (*)

let searchSpace =
    [
        for i in 0..100 do
        for j in 0..100 do
        for k in 0..100 do
        for l in 0..100 do
            if i + j + k + l = 100 then [|i; j; k; l|] 
    ]

searchSpace |> map (fun ps -> score input ps) |> maximum

// part two
let cals (a: int array array) (proportions: int array) =
    [| for i in 0..3 -> a[i][4] * proportions[i] |] |> sum

searchSpace
|> filter (fun ing -> cals input ing = 500)
|> map (fun ing -> score input ing)
|> maximum

#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let containers =
    [|
        33
        14
        18
        20
        45
        35
        16
        35
        1
        13
        18
        13
        50
        44
        48
        6
        24
        41
        30
        42
    |]
    
let indexes = [0..containers.Length - 1] |> sortByDescending (fun i -> containers[i])

let rec fill =
    fun filled n -> 
        [
            for i in indexes do
                if not (filled |> Set.contains i) then
                    let c = containers[i]
                    if n >= c then
                        yield! fill (filled.Add i) (n - c)
                    if n = c then
                        yield filled.Add i
        ] |> distinct
    |> memoizeN

let result = fill Set.empty 150 |> groupBy length

result |> minBy fst |> snd |> length
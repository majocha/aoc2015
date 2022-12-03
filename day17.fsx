#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let containers =
    [
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
    ] |> List.indexed |> Set

let rec fill =
    fun empty n -> 
        [
            for container in empty do
                let i, c = container
                if n >= c then
                    yield! fill (empty |> Set.remove container) (n - c)
                if n = c then
                    yield containers - (empty.Remove container)
        ]
    |> memoizeN

let result = fill containers 150 |> List.distinct |> List.groupBy Set.count

result |> List.minBy fst |> snd |> List.length
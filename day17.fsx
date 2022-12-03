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
    
let indexes = Set [0..containers.Length - 1]

let rec fill =
    fun empty n -> 
        [
            for i in empty do
                let c = containers[i]
                if n >= c then
                    yield! fill (empty |> Set.remove i) (n - c)
                if n = c then
                    yield indexes - (empty.Remove i)
        ]
    |> memoizeN

let result = fill indexes 150 |> List.distinct |> List.groupBy Set.count

result |> List.minBy fst |> snd |> List.length
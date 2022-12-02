#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input =
    [
        for s in System.IO.File.ReadAllLines "9.txt" do
            let p1, p2, d = sscanf "%s to %s = %d" s
            yield p1, (p2, d)
            yield p2, (p1, d)
    ]

let places = input |> List.map fst |> List.distinct

let connections =
    input
    |> List.groupBy fst
    |> List.map (fun (k, g) -> k, g |> List.map snd |> Map.ofList)
    |> Map.ofList

let getDistance visited =
    visited 
    |> List.pairwise 
    |> List.map (fun (p1, p2) -> connections[p1][p2])
    |> List.sum

let getPossible = function
    | Some p -> connections[p] |> Map.keys |> List.ofSeq
    | None -> places

let rec travel visited =
    if length visited = length places then Some(visited, getDistance visited)
    else
        let current = tryHead visited
        match getPossible current |> List.except visited with
        | [] -> None
        | many ->
            let dists = [
                    for p in many ->
                        travel (p::visited)
                ]
            match sequence dists with
            | None -> None
            | Some ds -> ds |> List.maxBy snd |> Some

travel []
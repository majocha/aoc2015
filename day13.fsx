#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = 
    System.IO.File.ReadAllLines "13.txt"
    |> Seq.map (sscanf "%s would %s %d happiness units by sitting next to %s.")
    |> Seq.map (fun (a, c, n: int, b) -> if c = "lose" then (a, b), -n else (a, b), n)
    |> List.ofSeq

let people = (input |> map (fst >> fst)) @ (input |> map (fst >> snd)) |> distinct

let happinessMap = input |> Map.ofList

let permutations ps =
    let rec loop placed available =
        [
            if available = [] then yield placed
            else 
                for a in available do
                    yield! loop (a::placed) (available |> List.except [a])
        ]
    loop [] ps

let total ps =
    [
        for p in ps |> List.pairwise do
            happinessMap |> Map.tryFind p
        for p in ps |> rev |> List.pairwise do
            happinessMap |> Map.tryFind p
        let head, last = ps[0], ps[ps.Length - 1]
        happinessMap |> Map.tryFind (head, last)
        happinessMap |> Map.tryFind (last, head)
    ] |> fold (fun t v -> t + (Option.defaultValue 0 v)) 0

("Kuba" :: people) |> permutations |> map total |> maximum
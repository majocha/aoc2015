#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let searchInput = """children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"""

let props = 
    searchInput |> String.split ["\n"] |> map (sscanf "%s: %d") |> Map

let input = System.IO.File.ReadAllLines "16.txt"

let parseProps ps =
    ps |> String.split [", "] |> map (sscanf "%s: %d")

let isMatch (p, n) =
    if not (props.ContainsKey p) then true
    else
        let v = props[p]
        match p with
        | "cats" | "trees" -> n > v           // part two
        | "pomeranians" | "goldfish" -> n < v // part two
        | _ -> n = v

let aunts = 
    input 
    |> map (sscanf "Sue %d: %s")
    |> map (fun (n, ps) -> n, parseProps ps |> toList)

aunts
|> filter (fun (n, ps) -> ps |> forall isMatch)

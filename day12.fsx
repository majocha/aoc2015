open System
let input = IO.File.ReadAllText "12.txt"

open System.Text.Json

let json = JsonDocument.Parse input

let root = json.RootElement

let hasRed (e:JsonElement) =
    [ 
        for p in e.EnumerateObject() do
            if p.Value.ValueKind = JsonValueKind.String then
                yield p.Value.ToString()
    ] |> List.contains "red"

let rec walk (e: JsonElement) =
    match e with
    | e when e.ValueKind = JsonValueKind.Object && hasRed e -> 0
    | e when e.ValueKind = JsonValueKind.Object ->
        [ for p in e.EnumerateObject() do walk p.Value ] |> List.sum
    | e when e.ValueKind = JsonValueKind.Array ->
        [ for v in e.EnumerateArray() do walk v ] |> List.sum
    | e when e.ValueKind = JsonValueKind.Number -> e.GetInt32()
    | _ -> 0
    
walk root
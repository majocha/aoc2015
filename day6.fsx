#r @"nuget: FSharpPlus, 1.2.5"

open FSharpPlus

let input = System.IO.File.ReadAllLines "6.txt"

let a = Array2D.create 1000 1000 0

let slice x1 y1 x2 y2 f =
    for x in [x1..x2] do
        for y in [y1..y2] do
            a[x,y] <- f (a[x,y])

for s in input do
    let instr, x1, y1, x2, y2 = sscanf "%s %d,%d through %d,%d" s
    match instr with
    | "turnon" -> slice x1 y1 x2 y2 (fun v -> v + 1)
    | "turnoff" -> slice x1 y1 x2 y2 (fun v -> if v > 0 then v - 1 else 0)
    | "toggle" -> slice x1 y1 x2 y2 (fun v -> v + 2)
    | _ -> failwith "wrong input"

a |> Seq.cast<int> |> Seq.sum
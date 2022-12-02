#r @"nuget: FSharpPlus, 1.2.5"

open FSharpPlus

let input =
    System.IO.File.ReadAllLines "2.txt" |> map (sscanf "%dx%dx%d") |> toList

let paper (l, w, h) =    
    let area x y = x * y
    let sides = [area l w; area w h; area h l]
    let extra = List.min sides
    extra + (sides |> List.sum) * 2

let ribbon (l, w, h) =
    let shortSides =  [l; w; h] |> List.sort |> List.rev |> List.tail
    let wrap = (List.sum shortSides) * 2
    let bow = l * w * h
    wrap + bow

input |> List.sumBy ribbon
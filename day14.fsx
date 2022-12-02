#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = 
    System.IO.File.ReadAllLines "14.txt"
    |> map (sscanf "%s can fly %d km/s for %d seconds, but then must rest for %d seconds.")

let profile (n, s, d, r) = [
    let mutable dist = 0
    for t in 1..2503 do
        for i in 1..d do
            dist <- dist + s
            yield dist
        for i in 1..r do yield dist
]

let score = Array.create 9 0
let profiles = input |> map profile
for t in 0..2502 do
    let positions = [ for i in 0..8 -> profiles[i][t] ]
    let best = positions |> maximum
    for i in 0..8 do if positions[i] = best then score[i] <- score[i] + 1

score |> maximum
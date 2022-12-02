let input = System.IO.File.ReadAllLines "5.txt"
let vowels = "aeiou" |> Seq.toList
let bad = ["ab"; "cd"; "pq"; "xy"]

let twice (s: string) =
    s |> Seq.pairwise |> Seq.exists (fun (a, b) -> a = b)

let threeVowels (s: string) =
    s |> Seq.filter (fun c -> "aeiou".Contains c) |> Seq.length >= 3

let isBad (s: string) =
    ["ab"; "cd"; "pq"; "xy"] |> List.exists (fun p -> s.Contains p)
    

let isNice s =
    (not (isBad s)) && (twice s) && (threeVowels s)

input |> Seq.countBy isNice



let rule2 s =
    s |> Seq.windowed 3 |> Seq.exists (fun a -> a[0] = a[2])

let rule1 (s: string) =
    let pair i =  s.[i+2..].Contains s.[i..i+1]
    [0..s.Length-2] |> List.exists pair

let isNice2 s = rule1 s && rule2 s

input |> Seq.countBy isNice2
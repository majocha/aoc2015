let inc (c: char) n =
    let c = (int c + n) |> char
    if c > 'z' then 'a', 1 else c, 0

let incP (s:string) =
    let rec loop current wrap output =
        let incremented, wrap = inc s[current] (wrap)
        if current = 0 then 
            incremented :: output
        else
            loop (current - 1) wrap (incremented :: output)
    loop (s.Length - 1) 1 []
    |> Array.ofList |> System.String

let hasThree s =
    let isConsecutive (cs: char array) =
        let a = cs |> Array.map int
        a[0] = a[1] - 1 && a[0] = a[2] - 2 
    s |> Seq.windowed 3 |> Seq.exists isConsecutive

let noForbiddenLetters (s: string) =
    not (s.Contains('i') || s.Contains('o') || s.Contains('l'))

let hasPairs s =
    let isPair (c, d) = c = d
    (s |> Seq.pairwise |> Seq.filter isPair |> Seq.distinct |> Seq.length) >= 2

let newPassword s =
    let rec loop s =
        if (hasThree s) && (noForbiddenLetters s) && (hasPairs s) then s
        else s |> incP |> loop
    s |> incP |> loop

newPassword "cqjxxyzz"
#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = [|3;1;1;3;3;2;2;1;1;3|]

let step (s: int array) =
    [|
        let mutable i = 0
        let mutable count = 1
        let mutable d = s[0]
        while i < s.Length - 1 do
            i <- i + 1
            if s[i] <> d then
                yield count
                yield d
                count <- 1
                d <- s[i]
            else count <- count + 1
        yield count
        yield d
    |]

let rec steps n s =
    if n <> 50 then
        printfn "Step %d: %A" n s
        steps (n +  1) (step s)
    else s

length (steps 0 input)

let input = System.IO.File.ReadAllText "1.txt"

let rec findFirst i f =
    if f = -1 then i else
        match input[i] with
        | '(' -> findFirst (i + 1) (f + 1)
        | ')' -> findFirst (i + 1) ( f - 1)
        | _ -> failwith "wrong character in input"

findFirst 0 0
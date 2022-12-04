let size = [1..100]

let neighbors =
    [
        for x in [-1..1] do
        for y in [-1..1] do
            if (x, y) <> (0,0) then x, y
    ]

let countOnNeghbors x y (grid: bool[,])  =
    let mutable count = 0
    for dx, dy in neighbors do
        if grid[x + dx, y + dy] then count <- count + 1
    count

let newState x y (grid: bool[,]) =
    let n = countOnNeghbors x y grid
    match grid[x,y] with
    | true when n = 2 || n = 3 -> true
    | false when n = 3 -> true
    | _ -> false

let stuckLights (grid: bool[,]) =  
    grid[1,1] <- true
    grid[100,100] <- true
    grid[1,100] <- true
    grid[100,1] <- true

let newGrid (old: bool[,]) =
    let grid = Array2D.create 102 102 false
    for x in size do
        for y in size do
            grid[x,y] <- newState x y old
    stuckLights grid
    grid

let initial =
    let input = System.IO.File.ReadAllLines "18.txt"
    let grid = Array2D.create 102 102 false
    for x in size do
        for y in size do 
            grid[x,y] <- input[x-1][y-1] = '#'
    stuckLights grid
    grid

let drawState (grid: bool[,]) =
    for x in size do
        for y in size do 
            if grid[x,y] then printf "#" else printf "."
        printfn ""

let rec step grid n =
    if n = 0 then grid else step (newGrid grid) (n - 1)

let result = 
    let final = step initial 100 
    seq {
        for i in size do
        for b in final[i, *] do
            if b then 1
    } |> Seq.sum
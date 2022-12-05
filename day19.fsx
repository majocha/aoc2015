#r @"nuget: FSharpPlus, 1.2.5"
open FSharpPlus

open System.Text.RegularExpressions

let replacements = System.IO.File.ReadAllLines "19.txt" |> map (sscanf "%s => %s")

let mol = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"

let countMatches input pattern = Regex.Matches(input, pattern).Count

let replaceNth input pattern replacement n =
    let mutable x = 0
    Regex.Replace(
        input , pattern, 
        fun m ->
            x <- x + 1
            if x = n then replacement else pattern
    )

let step input pattern replacement =
    [
        for n in 1..countMatches input pattern do
            replaceNth input pattern replacement n 
    ] |> List.distinct

let generateAll input replacements =
    [
    for pattern, replacement in replacements do
        yield! step input pattern replacement
    ] |> List.distinct

generateAll mol replacements |> length

let revRepl = replacements |> map (fun (a, b) -> b, a) |> sortByDescending (fst >> length)
let gen = (fun input -> generateAll input revRepl) |> memoizeN
#nowarn "40"
let rec fabrications =
    fun input (n: int) -> 
            seq {
                if input = "e" then
                    printfn "%d" n
                    yield n 
                else 
                    for s in gen input do
                        yield! fabrications s (n + 1)
            }
    |> memoizeN

let result = fabrications mol 0 |> distinct |> minimum

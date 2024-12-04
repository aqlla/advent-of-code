open System.Linq
open System.Text.RegularExpressions

let matches pattern str =
    Regex.Matches(str, pattern).Cast<Match>().Select(_.Value)
    
let ints = Seq.map int

System.IO.File.ReadLines("input/input.txt")
|> Seq.collect (matches "mul\(\d+,\d+\)")
|> Seq.map (matches "\d+" >> ints >> Seq.fold (*) 1)
|> Seq.sum
|> printfn "%d"

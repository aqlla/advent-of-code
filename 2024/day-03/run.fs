open System.Linq
open System.Text.RegularExpressions

let matches pattern str =
    Regex.Matches(str, pattern).Cast<Match>().Select(_.Value)

let p2Adjustments lines =
    lines
    |> Seq.fold (+) ""
    |> _.Split("do()")
    |> Seq.map (_.Split("don't()") >> Seq.head)

System.IO.File.ReadLines("input/input.txt")
|> p2Adjustments
|> Seq.collect (matches "mul\(\d+,\d+\)")
|> Seq.map (
    matches "\d+" >> Seq.map int >> Seq.fold (*) 1
)
|> Seq.sum
|> printfn "%i"

let split (delim: string) (str: string)=
    str.Split(delim)

let run =
    System.IO.File.ReadLines("./2024/day-01/f/input/part1.txt")
    |> Seq.map (split "   " >> fun i -> int i[0], int i[1])
    |> Seq.toList
    |> List.unzip
    |> fun (l, r) -> (List.sort l, List.sort r)
    ||> List.zip
    |> List.sumBy (fun (l, r) -> abs(l - r))
    |> printfn "%i"

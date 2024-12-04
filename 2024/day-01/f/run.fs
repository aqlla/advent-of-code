let split (delim: string) (str: string)=
    str.Split(delim)

let parseIntCols delim lines =
    lines
    |> Seq.map (split delim >> fun i -> int i[0], int i[1])
    |> Seq.toList
    |> List.unzip

let p1 input =
    input
    |> parseIntCols "   "
    |> fun (l, r) -> List.sort l, List.sort r
    ||> List.zip
    |> List.sumBy (fun (l, r) -> abs(l - r))

let countWhere pred xs =
    List.filter pred xs |> List.length
    
let countWhereEq value =
    countWhere (fun x -> x = value)
    
let p2Inefficient input =
    let left, right = input |> parseIntCols "   "
    left |> List.sumBy (fun x -> x * (countWhereEq x right))

let input =
    System.IO.File.ReadLines("input/part1.txt")

printfn $"%i{p1 input}"
printfn $"%i{p2Inefficient input}"
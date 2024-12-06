open System
open System.Linq
open System.Text.RegularExpressions
    
let matches pattern str =
    Regex.Matches(str, pattern).Cast<Match>().Select(_.Value)
    
let lineMatrix (lines: string seq) =
    let rows = Seq.length lines
    let cols = lines |> Seq.head |> Seq.length
    let ls = Seq.toArray lines
    Array2D.init rows cols (fun x y -> ls[x][y])
   
let len2 = function
    | 1 -> Array2D.length1
    | 2 -> Array2D.length2
    | _ -> failwith "dimension can only be 1 or 2"
        
let horizReverse mat =
    Array2D.init (len2 1 mat) (len2 2 mat) (fun y x -> mat[y, ^x])
    
let rec diag offset (mat: 'a[,]) =
    match offset with
    | o when o < 0 -> diag 0 mat[..^(-o), -o..]
    | o when o > 0 -> diag 0 mat[o.., ..^o]
    | _ ->
        let len = min (len2 1 mat) (len2 2 mat)
        Array.init len (fun i -> mat[i + offset, i])
        
let countXmas (mat: 'a[,]) =
    let len = min (len2 1 mat) (len2 2 mat)
    [
        [| -len..len |] |> Array.map (fun n -> diag n mat);
        [| -len..len |] |> Array.map (fun n -> diag n (horizReverse mat));
        [| 0 .. len2 1 mat - 1 |] |> Array.map (fun n -> mat[n, *]);
        [| 0 .. len2 2 mat - 1 |] |> Array.map (fun n -> mat[*, n]);
    ]
    |> Array.concat
    |> Array.map (fun chs ->
        String.Concat(chs) + " " + String.Concat(Array.rev chs) + " "
    )
    |> Array.fold (+) ""
    |> matches "XMAS"

let results =
    System.IO.File.ReadLines("input/input.txt")
    |> lineMatrix
    |> countXmas

results |> printfn "Result: %A"
Seq.length results |> printfn "  Len: %A"
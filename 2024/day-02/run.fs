open System
    
let andf f g x =
    f x && g x
    
let signOf n = n / abs n
let subTup (a, b) = a - b

let isSafe xs =
    Seq.pairwise xs
    |> Seq.map subTup
    |> Seq.toList
    |> function
        | a::rest when a <> 0 && abs a < 4  ->
            Seq.forall ((*) (signOf a) >> andf ((<) 0) ((>) 4)) rest
        | _ -> false
    
let isSafeDamp xs = function
    | _ when isSafe xs -> true
    | _ ->
       xs
       |> Seq.mapi (fun i _ -> i)
       |> Seq.exists (fun i -> xs |> Seq.removeAt i |> isSafe)

    
System.IO.File.ReadLines("input/input.txt")
|> Seq.map (_.Split(" ") >> Seq.map int)
|> Seq.sumBy (isSafeDamp >> Convert.ToInt16)
|> printfn "%i"

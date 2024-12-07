
let splitWhere f xs =
    let idx = xs |> Seq.findIndex f
    (xs |> Seq.take idx, xs |> Seq.skip (idx + 1))
    
let rules, manuals =
    System.IO.File.ReadLines("input/input.txt")
    |> splitWhere (Seq.length >> (=) 0)

let parsedRules =
    rules
    |> Seq.map _.Split("|", 2)
    |> Seq.map (fun ss -> (int ss[0], int ss[1]))

let parsedMans =
    manuals
    |> Seq.map (_.Split(",") >> Array.map int)
    
let intersects xs ys =
    Set.intersect (Set.ofList xs) (Set.ofList ys)
    |> Set.isEmpty
    |> not

let printAndPass fmt x =
    printfn fmt x
    x
    
let rec isOrdered rules (pages: 'a seq) =
    let preceding p =
        rules
        |> Seq.filter (fun r -> snd r = p)
        |> Seq.map fst
        |> Seq.toList
        
    match Seq.toList pages with
    | x::rest when preceding x |> intersects rest -> false
    | _::rest -> isOrdered rules rest
    | _ -> true
    
    
let results =
    parsedMans
    |> Seq.filter (parsedRules |> isOrdered)
    |> printAndPass "%A"
    |> Seq.map (Seq.toArray >> (fun ps -> ps[Array.length ps / 2]))
    |> printAndPass "%A"    
    |> Seq.sum
    
printfn $"%A{results}"
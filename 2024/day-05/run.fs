
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
    |> Seq.toList

let parsedMans =
    manuals
    |> Seq.map (_.Split(",") >> Array.map int >> Array.toList)
    
let intersect xs ys =
    Set.intersect (Set.ofList xs) (Set.ofList ys)
    
let intersects xs ys =
    intersect xs ys
    |> Set.isEmpty
    |> not

let printAndPass fmt x =
    printfn fmt x
    x
    
let preceding p rules =
    rules
    |> Seq.filter (fun r -> snd r = p)
    |> Seq.map fst
    |> Seq.toList  
  
let rec isOrdered rules pages =
    match pages with
    | x::rest when rules |> preceding x |> intersects rest -> false
    | _::rest -> isOrdered rules rest
    | _ -> true
    
    
let shouldPrecede rules cur p =
    rules
    |> List.exists (fun r -> fst r = p && snd r = cur)
    
let rec fixOrder rules (pages: 'a list) =
    match pages with
    | x::rest ->
        let pre, post =
            rest |> List.partition (shouldPrecede rules x)
        fixOrder rules pre @ x :: fixOrder rules post 
    | [] -> []
            
    
let results =
    parsedMans
    |> Seq.toList
    // |> List.filter (parsedRules |> isOrdered)
    |> List.filter (parsedRules |> isOrdered >> not)
    |> Seq.map (fixOrder parsedRules)
    |> Seq.map (Seq.toArray >> (fun ps -> ps[Array.length ps / 2]))
    |> Seq.sum
    
printfn $"%A{results}"
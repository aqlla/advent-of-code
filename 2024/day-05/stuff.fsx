
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
    
type collection<'a> =
    | List of 'a list
    | Seq of 'a seq
    
let asSet (cs: 'a collection) =
    match cs with
    | List(cs) -> Set.ofList
    | Seq(cs) -> Set.ofSeq
    
    
let intersects xs ys =
    Set.intersect (Set.ofList xs) (Set.ofList ys)
    |> Set.isEmpty
    |> not

    
let rec isOrdered rules (pages: 'a seq) =
    let preceding p =
        rules
        |> Seq.filter (fun r -> snd r = p)
        |> Seq.map fst
        |> Seq.toList
        
    match Seq.toList pages with
    | x::rest when preceding x |> intersects rest -> 
        isOrdered rules rest
    | _::_ -> false
    | _ -> true
    
let results =
    parsedMans
    |> Seq.filter (parsedRules |> isOrdered)
    |> Seq.map (Seq.toArray >> (fun ps -> ps[(Array.length ps + 1) / 2]))
    |> Seq.sum
    
printfn $"%A{Seq.toArray parsedRules}"
printfn $"%A{Seq.toArray parsedMans}"
printfn $"%A{results}"
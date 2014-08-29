namespace ProjectEuler.Problems

type Problem7() =
    
    let rec sieve ns =
        match ns with
        | (m::ms)    -> m :: sieve (List.filter (fun n -> n%m <> 0) ms)
        | []        -> []

    let flip f x y = f y x

    member this.Execute() =
        [1 .. 98000]
        |> sieve
        |> List.length
//        |> (flip List.nth) 10000
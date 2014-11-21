module Problem7_10001stPrime
    
    let rec private sieve ns =
        match ns with
        | (m::ms)    -> m :: sieve (List.filter (fun n -> n%m <> 0) ms)
        | []        -> []

    let private flip f x y = f y x

    let prime () =
        [2 .. 110000]
        |> sieve
        |> List.toSeq
        |> Seq.take 10001
        |> Seq.last
//        |> (flip List.nth) 10000
module Problem14_LongestCollatzSequence
//
//    let rec private bar input acc =
//        match input with
//        | []        -> acc
//        | i::rest   -> (computeTreeEndpoints i) :: acc

    let rec private collatzChain accumulatedChain cache start =
        if start = 1 then start :: accumulatedChain, 0
        else
            match Map.tryFind start cache with
            | Some length   -> accumulatedChain, length
            | None          -> let nextStart = if start % 2 = 0 then start / 2 else 3 * start + 1
                               collatzChain (start :: accumulatedChain) cache nextStart

    let private addToCache limit cache start length =
        if length >= limit then cache
        else Map.add start length cache

    let private filterCache (cache:Map<int,int>) =
        // Cock cock cock                                   // And > 1 !
        let canRemove k _ = Map.containsKey (2*k) cache && ((k-1) % 3 <> 0) || Map.containsKey ((k-1) / 3) cache
        Map.filter canRemove cache

    let private foo limit (cache:Map<int,int>) start =
        let cleanedCache =
            if (cache.Count > 240000) then
                filterCache cache
            else cache
        let chain, remainingLength = collatzChain [] cleanedCache start // e.g. chain = [8;16;5;10;20;40;13], 3
        (+) (remainingLength + 1)
        |> List.init (List.length chain)
        |> List.fold2 (addToCache limit) cleanedCache chain

    let result limit =
        let x =
            [1 .. limit-1]
            |> List.fold (foo limit) Map.empty
        x
        |> Map.filter (fun n _ -> n < limit)
        |> Map.toList
        |> List.maxBy snd

    // Build up a "cache" thing by applying n |-> (2n, Some (n-1)/3 or None) recursively to every n <- [1 .. million], keeping everything < million, taking the "end elements" in each chain
    // Then compute collatz chain lengths
namespace ProjectEuler.Problems

module Problem31_CoinsSums =

    let coinValues = [1; 2; 5; 10; 20; 50; 100; 200]
    
    type Coin =
        | OnePence
        | TwoPence
        | FivePence
        | TenPence
        | TwnetyPence
        | FiftyPence
        | OnePound
        | TwoPounds

        member x.value coin =
            match coin with
            | OnePence      -> 1
            | TwoPence      -> 2
            | FivePence     -> 5
            | TenPence      -> 10
            | TwnetyPence   -> 20
            | FiftyPence    -> 50
            | OnePound      -> 100
            | TwoPounds     -> 200


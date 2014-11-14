namespace ProjectEuler.Problems

type Problem9_SpecialPythagoreanTriplet() =

    let testTriplet a b c = a*a + b*b = c*c && a+b+c = 1000

    member this.Execute() =
        List.head [ for a in [1 .. 250] do for b in [a+1 .. 500] do for c in [b+1 .. 750] do if testTriplet a b c then yield a*b*c ]
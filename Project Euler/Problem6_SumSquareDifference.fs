namespace ProjectEuler.Problems

type Problem6_SumSquareDifference() =

    let source = [1 .. 100]

    let square n = n * n

    member this.Execute() = (square (Seq.sum source)) - (Seq.sum (Seq.map square source))
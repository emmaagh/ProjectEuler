namespace ProjectEuler.Problems

type Problem2() =

    let sum xs filter bound = Seq.sum <| Seq.takeWhile (fun n -> n < bound) (Seq.filter filter xs)

    let fibonacciNumbers = Seq.unfold (fun (a, b) -> Some (b, (b, a + b))) (0,1)

    let isEven n = n%2 = 0

    member this.Execute() = sum fibonacciNumbers isEven 4000000

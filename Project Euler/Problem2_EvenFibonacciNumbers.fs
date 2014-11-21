module Problem2EvenFibonacciNumbers

    let private sum xs filter bound = Seq.sum <| Seq.takeWhile (fun n -> n < bound) (Seq.filter filter xs)

    let private fibonacciNumbers = Seq.unfold (fun (a, b) -> Some (b, (b, a + b))) (0,1)

    let private isEven n = n%2 = 0

    let execute () = sum fibonacciNumbers isEven 4000000

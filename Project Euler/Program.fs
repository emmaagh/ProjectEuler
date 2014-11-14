open ProjectEuler.Problems

[<EntryPoint>]
let main argv = 
    let result = Problem7_10001stPrime.prime ()
    printfn "%A" result
    let x = System.Console.ReadKey()
    0
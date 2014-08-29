open ProjectEuler.Problems

[<EntryPoint>]
let main argv = 
    let result = Problem14_LongestCollatzSequence.result 120000
    printfn "%A" result
//    printfn "%A" (result * 100.0)
    let x = System.Console.ReadKey()
    0
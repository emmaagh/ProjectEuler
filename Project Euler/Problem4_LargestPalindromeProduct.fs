namespace ProjectEuler.Problems

open System;

type Problem4_LargestPalindromeProduct() =

    let equalEnds (s:char[]) = s.[0].Equals(s.[s.Length-1])

    let snipEnds (s:char[]) = s.[1 .. s.Length-2]

    let rec isPalindrome (s:char[]) =
        match s.Length with
        | 0 | 1     -> true
        | otherwise -> equalEnds s && isPalindrome <| snipEnds s

    let isPalindrome n = isPalindrome(n.ToString().ToCharArray())

    let cartesianProduct ns = seq { for n in ns do for m in ns -> (n, m) }

    let products ns = 
        cartesianProduct ns
        |> Seq.filter (fun (n, m) -> n <= m)
        |> Seq.map (fun (n, m) -> n*m)

    member this.Execute() =
        products [1000 .. 9999]
        |> Seq.filter isPalindrome
        |> Seq.max
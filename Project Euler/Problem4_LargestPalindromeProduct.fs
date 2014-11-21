module Problem4_LargestPalindromeProduct

    let private equalEnds (s:char[]) = s.[0].Equals(s.[s.Length-1])

    let private snipEnds (s:char[]) = s.[1 .. s.Length-2]

    let rec private isPalindrome (s:char[]) =
        match s.Length with
        | 0 | 1     -> true
        | otherwise -> equalEnds s && isPalindrome <| snipEnds s

    let private isPalindrome' n = isPalindrome(n.ToString().ToCharArray())

    let private cartesianProduct ns = seq { for n in ns do for m in ns -> (n, m) }

    let private products ns = 
        cartesianProduct ns
        |> Seq.filter (fun (n, m) -> n <= m)
        |> Seq.map (fun (n, m) -> n*m)

    let execute () =
        products [1000 .. 9999]
        |> Seq.filter isPalindrome'
        |> Seq.max
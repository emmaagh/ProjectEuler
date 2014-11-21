module NumberTheory

    open System

    type Fraction =
        | Fraction of int * int

        static member (*) (Fraction (a,b), Fraction (c,d)) = Fraction (a*c, b*d)
        static member (+) (Fraction (a,b), Fraction (c,d)) = Fraction (a*d + b*c, b*d)
       
    let rec private highestCommonFactor a (b:int) =
        let larger, smaller = Math.Max(a,b), Math.Min(a,b)
        let rem = larger % smaller
        if rem = 0 then smaller
        else highestCommonFactor b rem

    let reduce (Fraction (a,b)) = let hcf = highestCommonFactor a b
                                  Fraction (a/hcf, b/hcf)

    let inverse (Fraction (a,b)) = Fraction (b,a)



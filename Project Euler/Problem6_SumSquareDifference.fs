module Problem6_SumSquareDifference

    let private source = [1 .. 100]

    let private square n = n * n

    let execute () = (square (Seq.sum source)) - (Seq.sum (Seq.map square source))
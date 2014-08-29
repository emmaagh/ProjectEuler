namespace ProjectEuler.Problems

open System

module Problem155_CountingCapacitorCircuits =

    type Fraction =
        | Fraction of int * int

        static member (*) (Fraction (a,b), Fraction (c,d)) = Fraction (a*c, b*d)
        static member (+) (Fraction (a,b), Fraction (c,d)) = Fraction (a*d + b*c, b*d)
    
    let rec private highestCommonFactor a (b:int) =
        let larger, smaller = Math.Max(a,b), Math.Min(a,b)
        let rem = larger % smaller
        if rem = 0 then smaller
        else highestCommonFactor b rem

    let private reduce f =
        match f with
        | Fraction (a,b)    -> let hcf = highestCommonFactor a b
                               Fraction (a/hcf, b/hcf)

    let private inverse f =
        match f with
        | Fraction (a,b)    -> Fraction (b,a)

    let private seriesCircuit capA capB =
        (inverse capA) + (inverse capB)
        |> inverse
        |> reduce

    let private parallelCircuit capA capB = reduce (capA + capB)

    let private circuits' otherCircuits cap =
        List.map (seriesCircuit cap) otherCircuits
        |> List.append (List.map (parallelCircuit cap) otherCircuits)

    let private valueByKey key =
        List.find (fst >> ((=) key))
        >> snd

    let private normaliseCircuitsCollection =
        List.concat
        >> Seq.distinct
        >> Seq.toList

    let private circuits sizeUnitA sizeUnitB smallerCircuits =
        let unitACapacitances = valueByKey sizeUnitA smallerCircuits
        let unitBCapacitances = valueByKey sizeUnitB smallerCircuits
        unitACapacitances
        |> List.map (circuits' unitBCapacitances)
        |> normaliseCircuitsCollection

    let rec private circuitsBySize size : (int * Fraction list) list =
        match size with
        | 1                 -> [(1,[Fraction (60,1)])]
        | _ when size > 1   -> let smallerCircuits = circuitsBySize <| size-1
                               let newCircuits =
                                   List.init (size/2) (fun n -> (n+1, size-n-1))
                                   |> List.map (fun (sizeUnitA,sizeUnitB) -> circuits sizeUnitA sizeUnitB smallerCircuits)
                                   |> normaliseCircuitsCollection
                                   |> (fun caps -> (size, caps))
                               newCircuits :: smallerCircuits
        | _                 -> failwith "shouldn't be calculating resistance of circuits with non-positive number of resisters"

    let result size =
        circuitsBySize size
        |> List.map (fun (_, caps) -> caps)
        |> List.concat
        |> Seq.distinct
        |> Seq.length
module Problem155_CountingCapacitorCircuits_V2

    open NumberTheory
    
    let private seriesCircuit capA capB =
        (inverse capA) + (inverse capB)
        |> inverse
        |> reduce

    let private parallelCircuit capA capB = reduce (capA + capB)

    let private circuits' otherCircuits cap =
        List.map (seriesCircuit cap) otherCircuits
        |> List.append (List.map (parallelCircuit cap) otherCircuits)

    let private normaliseCircuitsCollection =
        List.concat
        >> Seq.distinct
        >> Seq.toList

    let private circuits sizeUnitA sizeUnitB smallerCircuits =
        let unitACapacitances = Map.find sizeUnitA smallerCircuits
        let unitBCapacitances = Map.find sizeUnitB smallerCircuits
        unitACapacitances
        |> List.map (circuits' unitBCapacitances)
        |> normaliseCircuitsCollection

    let rec private circuitsByCapacitorCount = function
        | 1                    -> [(1,[Fraction (60,1)])] |> Map.ofList
        | size when size > 1   -> let smallerCircuits = circuitsByCapacitorCount <| size-1
                                  let newCircuits =
                                      List.init (size/2) (fun n -> n+1, size-n-1)
                                      |> List.map (fun (sizeUnitA,sizeUnitB) -> circuits sizeUnitA sizeUnitB smallerCircuits)
                                      |> normaliseCircuitsCollection
                                  Map.add size newCircuits smallerCircuits
        | _                    -> failwith "shouldn't be calculating resistance of circuits with non-positive number of resistors"

    let result size =
        circuitsByCapacitorCount size
        |> Map.toList
        |> List.map snd
        |> List.concat
        |> Seq.distinct
        |> Seq.length
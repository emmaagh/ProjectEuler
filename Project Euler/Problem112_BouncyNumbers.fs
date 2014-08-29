namespace ProjectEuler.Problems

open System

module Problem112_BouncyNumbers =
    
    let private inclusiveMinToInclusiveMax min max =
        if max - min + 1 > 0 then
            List.init (max - min + 1) ((+) min)
        else
            []

    let private inclusiveMinToExclusiveMax min max =
        if min < max then
            List.init (max - min) ((+) min)
        else
            []

    let private exclusiveMinToInclusiveMax min max =
        if min < max then
            List.init (max - min) ((+) (min + 1))
        else
            []

    let private minToNine min = inclusiveMinToInclusiveMax min 9

    let private zeroToMax = inclusiveMinToExclusiveMax 0

    let private appendZeroToMax max tail = List.map (fun i -> i :: tail) (zeroToMax max)

    let private appendZeroToNine tail = List.map (fun i -> i :: tail) (minToNine 0)
    
    let rec private decimalPlaces n =
        if n < 10 then [n]
        else (n % 10) :: (decimalPlaces <| n/10)

    let private increasersFromPreviouslyConstSequences = minToNine 1 |> List.map (((+) 1) >> minToNine) |> List.concat

    let private decreasersFromPreviouslyConstSequences = minToNine 1 |> List.map zeroToMax |> List.concat

    let glue previousDigits newDigit = newDigit :: previousDigits

    let private deriveNextStageBounciesFromBouncies bouncies =
        bouncies
        |> List.map (fun bouncer -> appendZeroToNine bouncer)
        |> List.concat

    let private deriveNextStageBounciesFromIncreasers increasers =
        let bouncyNextDigitOptions increaser = exclusiveMinToInclusiveMax (List.nth increaser 0) 9
        increasers
        |> List.map (fun increaser -> List.map (glue increaser) (bouncyNextDigitOptions increaser))
        |> List.concat

    let private deriveNextStageBounciesFromDecreasers decreasers =
        let bouncyNextDigitOptions decreaser = inclusiveMinToExclusiveMax 1 (List.nth decreaser 0)
        decreasers
        |> List.map (fun decreaser -> List.map (glue decreaser) (bouncyNextDigitOptions decreaser))
        |> List.concat

    let private deriveConstanters dps =
        inclusiveMinToInclusiveMax 1 9
        |> List.map (fun i -> List.replicate dps [i])
        |> List.concat

    let private deriveNextStageIncreasersFromIncreasers increasers =
        let increasingNextDigitOptions increaser = inclusiveMinToInclusiveMax 1 (List.nth increaser 0)
        increasers
        |> List.map (fun increaser -> List.map (glue increaser) (increasingNextDigitOptions increaser))
        |> List.concat

    let private deriveNextStageDecreasersFromDecreasers decreasers =
        let decreasingNextDigitOptions decreaser = inclusiveMinToInclusiveMax (List.nth decreaser 0) 9
        decreasers
        |> List.map (fun decreaser -> List.map (glue decreaser) (decreasingNextDigitOptions decreaser))
        |> List.concat

    let private deriveNextStageIncreasersFromConstanters constanters =
        let increasingNextDigitOptions constanter = inclusiveMinToExclusiveMax 1 (List.nth constanter 0)
        constanters
        |> List.map (fun increaser -> List.map (glue increaser) (increasingNextDigitOptions increaser))
        |> List.concat

    let private deriveNextStageDecreasersFromConstanters constanters =
        let decreasingNextDigitOptions constanter = exclusiveMinToInclusiveMax (List.nth constanter 0) 9
        constanters
        |> List.map (fun decreaser -> List.map (glue decreaser) (decreasingNextDigitOptions decreaser))
        |> List.concat

    let private bouncyNumbers maxDecimalPlaces =
        let rec bouncyIncreasingDecreasingTotal maxDps =
            if maxDps = 2 then (
                [],
                List.map (fun i -> [i]) increasersFromPreviouslyConstSequences,
                List.map (fun i -> [i]) decreasersFromPreviouslyConstSequences,
                [])
            else
                let (bouncies, increasers, decreasers, smallerBouncies) = bouncyIncreasingDecreasingTotal <| maxDps - 1
                let newBouncies =
                    deriveNextStageBounciesFromBouncies bouncies
                    |> List.append (deriveNextStageBounciesFromIncreasers increasers)
                    |> List.append (deriveNextStageBounciesFromDecreasers decreasers)
                let newConstanters = deriveConstanters maxDps
                let newIncreasers =
                    deriveNextStageIncreasersFromIncreasers increasers
                    |> List.append (deriveNextStageIncreasersFromConstanters newConstanters)
                let newDecreasers =
                    deriveNextStageDecreasersFromDecreasers decreasers
                    |> List.append (deriveNextStageDecreasersFromConstanters newConstanters)
                (newBouncies, newIncreasers, newDecreasers, List.append smallerBouncies newBouncies)
        if maxDecimalPlaces <= 2 then []
        else
            let (_,_,_,bouncies) = bouncyIncreasingDecreasingTotal maxDecimalPlaces
            bouncies

    let private checkSmaller benchmark number =
        if List.length number < List.length benchmark then
            true
        else if List.length number > List.length benchmark then
            false
        else
            let rec helper n b =
                match n, b with
                | [], []            -> false
                | n'::n'', b'::b''  -> if n' < b' then
                                           true
                                       else if n' > b' then
                                           false
                                       else
                                           helper n'' b''
                | _                 -> failwith "Huh?"
            helper number benchmark

    let bouncyProportion n =
        let decimalPlaces = decimalPlaces n
        let maxDecimalPlaces = List.length decimalPlaces
        let bouncyTotal =
            bouncyNumbers maxDecimalPlaces
            |> List.filter (checkSmaller decimalPlaces)
            |> List.length
        (float) bouncyTotal / (float) n
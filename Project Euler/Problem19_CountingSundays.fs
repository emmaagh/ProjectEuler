module Problem19_CountingSundays

    let isLeapYear year =
        match year % 400, year % 100, year % 4 with
        | 0,_,_     -> true
        | _,0,_     -> false
        | _,_,0     -> true
        | _         -> false

    let monthLength year month =
        match month with
        | 4 | 6 | 9 | 11    -> 30
        | 2                 -> if isLeapYear year then 29 else 28
        | _                 -> 31

    let nextFirstDay year month day = (day + monthLength year month) % 7

    let nextFirst date =
        let (year, month, day) = date
        match month with
        | 12    -> (year + 1,   1,          nextFirstDay year month day)
        | _     -> (year,       month + 1,  nextFirstDay year month day)

    let rec firstDaysOfMonth startDate endYear =
        let (startYear,_,_) = startDate
        let next = nextFirst startDate
        if startYear < endYear then startDate :: firstDaysOfMonth (next) endYear
        else []

    let execute =
        firstDaysOfMonth (1900,1,0) 2001
        |> List.filter (fun (y,_,_) -> y > 1900)
        |> List.filter (fun (_,_,d) -> d = 6)
        |> List.length
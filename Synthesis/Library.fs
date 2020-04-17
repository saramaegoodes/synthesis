module Synthesis

let abelar n = n>12 && n<3097 && n%12=0
    //failwith "Not implemented"

let area b h = 
    match b<0.0 || h<0.0 with
    | true -> failwith "negative numbers"
    | false -> b * h * 0.5

    //failwith "Not implemented"

let zollo a =
    match a>=0 with
    | true -> a * 2
    | false -> a * -1
    //failwith "Not implemented"

let min a b =
    match a<b with
    | true -> a
    | false -> b
    //failwith "Not implemented"

let max a b =
    match a>b with
    | true -> a
    | false -> b
    //failwith "Not implemented"

let ofTime h m s =
    h * 3600 + m * 60 + s
    //failwith "Not implemented"

let toTime t =
    match t<0 with
    |false -> (t/3600,(t%3600)/60,((t%3600)%60))
    |true -> (0,0,0)

    //failwith "Not implemented"

let digits x =
    let rec sumDig v count =
        match v<>0 with
        |true -> sumDig (v/10) (count + 1) |false -> count  
    
    match x = 0 with |true -> 1 |_ -> sumDig x 0


    //failwith "Not implemented"

let minmax (num1, num2, num3, num4) = 
    
    (min(min num1 num2) (min num3 num4), max (max num1 num2) (max num3 num4))

    //failwith "Not implemented"

let isLeap a=
    match a >= 1582  with
    |false -> failwith "shouldfail"
    |true-> a % 4 = 0 && not(a % 100 = 0) || a % 400 = 0    
    //failwith "Not implemented"

let month a =
    match a<=12 && a>=1 with 
    |false -> failwith "shouldFail"
    |true -> match a with 
    |1 -> ("January", 31) |2 -> ("February", 28) 
    |3 -> ("March", 31) |4 -> ("April", 30) 
    |5 -> ("May", 31) |6 -> ("June", 30) 
    |7 -> ("July", 31) |8 -> ("August", 31)
    |9 -> ("September", 30) |10 -> ("October", 31)
    |11 -> ("November", 30)|12 -> ("December", 31)
    //failwith "Not implemented"

let toBinary a =
    let rec intobin a inc=
        match a with
        |1|0 -> string(a) + inc
        |b -> intobin (a / 2) (string(a % 2) + inc)
    match a >= 0 with
    |true -> intobin a "" 
    |false -> failwith "shouldFail"
    //failwith "Not implemented"

let bizFuzz n =
    match n<1 with 
    |true -> 0,0,0
    |false -> n/3,n/5,n/15
    //failwith "Not implemented"

let monthDay d y =
    let leapYear = isLeap y
    match d >= 1 && (leapYear && d <= 366 || not leapYear && d <= 365) with
    |false -> failwith "shouldFail"
    |_ -> 
        let rec iterate curr remain = 
            let name,day = 
                match leapYear,curr,month curr with
                |true, 2, (name,day) -> name, day + 1
                |_, _, v-> v
            match remain <= day with
            |true -> name
            |_ -> iterate (curr + 1) (remain - day)
        iterate 1 d
    //failwith "Not implemented"

let coord (x1, y1) =
    let sqrt n = 
        let rec calculate guess i =
            match i with 
            |10 -> guess
            |_ -> calculate ((guess + n/guess) / 2.0)(i + 1)
        calculate (n/2.0)0

    let dist (x2,y2)=
        let d1,d2 = x1-x2, y1-y2
        sqrt (d1*d1 + d2*d2)

    let within (x_1, y_1) width height = 
        let x_coord, y_coord = (x_1 + width),(y_1 - height)
        x1>=x_1 && y1 <= y_1 && x1 <= x_coord && y1 >= y_coord
    dist, within

    //failwith "Not implemented"
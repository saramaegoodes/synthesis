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

let minmax _ =

    failwith "Not implemented"

let isLeap a =
    match a >= 1582  with
    |false -> failwith "shouldfail"
    |true  -> a % 4 = 0 && not(a % 100 = 0) || a % 400 = 0    
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
    let rec intotobin a =
        match a with 
        |b -> (a % 2 + 10 * (intotobin(a/2)))
    match a>=0 with
    |true -> match a with |0 -> "0" |1 -> "1" |b -> '"' + b + '"'
    |false -> failwith "freh"
    
    //let days a =
    //    match a with
     //   |0 -> "0"
    //    |   _ -> "1"
     //   |b -> intotobin a

    //|b -> 


    //let other a =
     //   match a with 
     //   |0 -> "0"
     //   |1 -> "1"
   // match (a<>0) && (a<>1) with
   // |true -> intotobin a 
   // |false -> other a
    ///|0 -> "0"
    //|1 -> "1"
    
    //let rec inttobin a = a%2 + 10 * (inttobin(a/2))
    
        //match i with
        //|0 -> "0"
       // |1 -> "1"
       // |_ -> let bit = string (i%2)
        //       (inttobin (i/2)) + bit

    //failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"
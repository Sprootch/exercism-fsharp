module ListOps

let rec foldl folder state list =
    failwith "You need to implement this function."

let rec foldr folder state list =
    failwith "You need to implement this function."

let length list =
    let mutable i = 0

    for _ in list do
        i <- i + 1

    i

let reverse list =
    failwith "You need to implement this function."

let map f list =
    failwith "You need to implement this function."

let filter f list =
    failwith "You need to implement this function."

let append xs ys =
    failwith "You need to implement this function."

let concat xs =
    failwith "You need to implement this function."

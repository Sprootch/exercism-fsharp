module BottleSong

open System


let capitalize (text: string) =
    (Char.ToUpper(text[0]) |> string) + text[1..]

let ofBottles =
    let toEnglish =
        function
        | 10 -> "ten"
        | 9 -> "nine"
        | 8 -> "eight"
        | 7 -> "seven"
        | 6 -> "six"
        | 5 -> "five"
        | 4 -> "four"
        | 3 -> "three"
        | 2 -> "two"
        | 1 -> "one"
        | 0 -> "zero"
        | n -> failwith $"todo: {n}"

    function
    | 0 -> "no green bottles"
    | 1 -> "one green bottle"
    | n -> $"{n |> toEnglish} green bottles"

let firstLine number =
    $"{number |> ofBottles |> capitalize} hanging on the wall,"

let secondLine = "And if one green bottle should accidentally fall,"

let lastLine number =
    $"There'll be {number |> ofBottles} hanging on the wall."

let recite startBottles takeDown =
    [ firstLine startBottles
      firstLine startBottles
      secondLine
      lastLine (startBottles - 1) ]

module OcrNumbers

let toDigit =
    function
    | [ " _ "; "| |"; "|_|"; "   " ] -> "0"
    | [ "   "; "  |"; "  |"; "   " ] -> "1"
    | [ " _ "; " _|"; "|_ "; "   " ] -> "2"
    | [ " _ "; " _|"; " _|"; "   " ] -> "3"
    | [ "   "; "|_|"; "  |"; "   " ] -> "4"
    | [ " _ "; "|_ "; " _|"; "   " ] -> "5"
    | [ " _ "; "|_ "; "|_|"; "   " ] -> "6"
    | [ " _ "; "  |"; "  |"; "   " ] -> "7"
    | [ " _ "; "|_|"; "|_|"; "   " ] -> "8"
    | [ " _ "; "|_|"; " _|"; "   " ] -> "9"
    | _ -> "?"

let splitIntoDigits (rows: string list) =
    // Convertit chaque ligne en tableau de caractères
    let charRows = rows |> List.map Seq.toArray

    // Nombre de colonnes par "chiffre" (3 dans ton cas)
    let digitWidth = 3

    // Nombre total de chiffres dans la ligne
    let numDigits = (charRows.Head.Length + 1) / digitWidth

    // Pour chaque chiffre, extraire les 4 colonnes correspondantes
    [ for i in 0 .. numDigits - 1 ->
          [ for row in charRows ->
                let start = i * digitWidth
                let sliceLength = min digitWidth (row.Length - start)
                new string (row[start .. start + sliceLength - 1]) ] ]

let convert input =
    if (input |> List.length) <> 4 then
        None
    else if input |> List.exists (fun (line: string) -> line.Length <> 4) |> not then
        None
    else
        input |> splitIntoDigits |> List.map toDigit |> List.toSeq |> String.concat "" |> Some
        // input |> toDigit |> Some

let rows =
        [ "       _     _        _  _ ";
          "  |  || |  || |  |  || || |";
          "  |  ||_|  ||_|  |  ||_||_|";
          "                           " ]

let result = [
    [ "   "; "  |"; "  |"; "   " ]
    [ "   "; "  |"; "  |"; "   " ]
    [ " _ "; "| |"; "|_|"; "   " ]
    [ "   "; "  |"; "  |"; "   " ]
    [ " _ "; "| |"; "|_|"; "   " ]
    [ "   "; "  |"; "  |"; "   " ]
    [ " _ "; "| |"; "|_|"; "   " ]
    [ " _ "; "| |"; "|_|"; "   " ]
]

// rows |> List.map (Seq.toList) |> List.transpose

rows |> splitIntoDigits |> List.map toDigit |> List.toSeq |> String.concat ""

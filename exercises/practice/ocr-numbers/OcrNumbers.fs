module OcrNumbers

let convert input =
    if (input |> List.length) <> 4 then
        None
    else if input |> List.exists (fun (line: string) -> line.Length <> 4) |> not then
        None
    else
        match input with
        | [ " _ "; "| |"; "|_|"; "   " ] -> Some "0"
        | [ "   "; "  |"; "  |"; "   " ] -> Some "1"
        | [ " _ "; " _|"; "|_ "; "   " ] -> Some "2"
        | [ " _ "; " _|"; " _|"; "   " ] -> Some "3"
        | [ "   "; "|_|"; "  |"; "   " ] -> Some "4"
        | [ " _ "; "|_ "; " _|"; "   " ] -> Some "5"
        | [ " _ "; "|_ "; "|_|"; "   " ] -> Some "6"
        | [ " _ "; "  |"; "  |"; "   " ] -> Some "7"
        | [ " _ "; "|_|"; "|_|"; "   " ] -> Some "8"
        | [ " _ "; "|_|"; " _|"; "   " ] -> Some "9"
        | _ -> Some "?"

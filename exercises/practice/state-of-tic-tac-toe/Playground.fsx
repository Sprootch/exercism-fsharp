type EndGameState =
    | Win
    | Ongoing
    | Draw

type GameError =
    | ConsecutiveMovesBySamePlayer
    | WrongPlayerStarted
    | MoveMadeAfterGameWasDone

let board =
        array2D [ ['X'; 'X'; 'O'];
                  ['O'; 'X'; 'X'];
                  ['X'; 'O'; 'O'] ]

let rowWins =
    [ for row in 0 .. ((board |> Array2D.length2) - 1) ->
          board[row, *] |> Array.forall (fun cell -> cell = 'X')
          || board[row, *] |> Array.forall (fun cell -> cell = 'O') ]

let colWins =
    [ for col in 0 .. ((board |> Array2D.length1) - 1) ->
          board[*, col] |> Array.forall (fun cell -> cell = 'X')
          || board[*, col] |> Array.forall (fun cell -> cell = 'O') ]

let result = rowWins @ colWins |> List.countBy id

match result with
| [ (false, _); (true, j) ] -> if (j = 1) then Win else Ongoing
| [ (true, j); (false, _) ] -> if (j = 1) then Win else Ongoing
| [ (false, _) ] -> Draw
| _ -> failwith "error"

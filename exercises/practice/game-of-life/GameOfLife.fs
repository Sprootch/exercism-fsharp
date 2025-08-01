module GameOfLife

let getLivingNeighbours row col cells =
    let isInbound (row, col) =
        (row >= 0 && row < (cells |> Array2D.length1))
        && (col >= 0 && col < (cells |> Array2D.length2))

    List.allPairs [ - 1 .. 1 ] [ - 1 .. 1 ]
    |> List.filter (fun (r, c) -> (r = 0 && c = 0) |> not)
    |> List.map (fun (r, c) -> (r + row, c + col))
    |> List.filter isInbound
    |> List.map (fun (row, col) -> cells[row, col])
    |> List.filter (fun c -> c = 1)
    |> List.length

let nextGeneration row col cell cells =
    let livingNeighbours = cells |> getLivingNeighbours row col

    if (cell = 0 && livingNeighbours = 3) then
        1
    elif (cell = 1 && (livingNeighbours = 2 || livingNeighbours = 3)) then
        1
    else
        0

let tick (input: int[,]) =
    input |> Array2D.mapi (fun x y cell -> nextGeneration x y cell input)

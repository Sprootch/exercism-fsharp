module ListOpsTests

open FsUnit.Xunit
open Xunit

open ListOps

[<Fact>]
let ``append empty lists`` () =
    append [] [] |> should be Empty

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``append list to empty list`` () =
    append [] [1; 2; 3; 4] |> should equal [1; 2; 3; 4]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``append empty list to list`` () =
    append [1; 2; 3; 4] [] |> should equal [1; 2; 3; 4]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``append non-empty lists`` () =
    append [1; 2] [2; 3; 4; 5] |> should equal [1; 2; 2; 3; 4; 5]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``concat empty list`` () =
    concat [] |> should be Empty

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``concat list of lists`` () =
    concat [[1; 2]; [3]; []; [4; 5; 6]] |> should equal [1; 2; 3; 4; 5; 6]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``concat list of nested lists`` () =
    concat [[[1]; [2]]; [[3]]; [[]]; [[4; 5; 6]]] |> should equal [[1]; [2]; [3]; []; [4; 5; 6]]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``filter empty list`` () =
    filter (fun acc -> acc % 2 = 1) [] |> should be Empty

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``filter non-empty list`` () =
    filter (fun acc -> acc % 2 = 1) [1; 2; 3; 5] |> should equal [1; 3; 5]

[<Fact>]
let ``length empty list`` () =
    length [] |> should equal 0

[<Fact>]
let ``length non-empty list`` () =
    length [1; 2; 3; 4] |> should equal 4

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``map empty list`` () =
    map (fun acc -> acc + 1) [] |> should be Empty

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``map non-empty list`` () =
    map (fun acc -> acc + 1) [1; 3; 5; 7] |> should equal [2; 4; 6; 8]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``foldl empty list`` () =
    foldl (fun acc el -> el * acc) 2 [] |> should equal 2

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``foldl direction independent function applied to non-empty list`` () =
    foldl (fun acc el -> el + acc) 5 [1; 2; 3; 4] |> should equal 15

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``foldr empty list`` () =
    foldr (fun acc el -> el * acc) 2 [] |> should equal 2

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``foldr direction independent function applied to non-empty list`` () =
    foldr (fun acc el -> el + acc) 5 [1; 2; 3; 4] |> should equal 15

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``reverse empty list`` () =
    reverse [] |> should be Empty

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``reverse non-empty list`` () =
    reverse [1; 3; 5; 7] |> should equal [7; 5; 3; 1]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``reverse list of lists is not flattened`` () =
    reverse [[1; 2]; [3]; []; [4; 5; 6]] |> should equal [[4; 5; 6]; []; [3]; [1; 2]]

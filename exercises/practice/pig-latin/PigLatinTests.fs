module PigLatinTests

open FsUnit.Xunit
open Xunit

open PigLatin

[<Fact>]
let ``Word beginning with a`` () =
    translate "apple" |> should equal "appleay"

[<Fact>]
let ``Word beginning with e`` () =
    translate "ear" |> should equal "earay"

[<Fact>]
let ``Word beginning with i`` () =
    translate "igloo" |> should equal "iglooay"

[<Fact>]
let ``Word beginning with o`` () =
    translate "object" |> should equal "objectay"

[<Fact>]
let ``Word beginning with u`` () =
    translate "under" |> should equal "underay"

[<Fact>]
let ``Word beginning with a vowel and followed by a qu`` () =
    translate "equal" |> should equal "equalay"

[<Fact>]
let ``Word beginning with p`` () =
    translate "pig" |> should equal "igpay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with k`` () =
    translate "koala" |> should equal "oalakay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with x`` () =
    translate "xenon" |> should equal "enonxay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with q without a following u`` () =
    translate "qat" |> should equal "atqay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with ch`` () =
    translate "chair" |> should equal "airchay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with qu`` () =
    translate "queen" |> should equal "eenquay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with qu and a preceding consonant`` () =
    translate "square" |> should equal "aresquay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with th`` () =
    translate "therapy" |> should equal "erapythay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with thr`` () =
    translate "thrush" |> should equal "ushthray"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with sch`` () =
    translate "school" |> should equal "oolschay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with yt`` () =
    translate "yttria" |> should equal "yttriaay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Word beginning with xr`` () =
    translate "xray" |> should equal "xrayay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Y is treated like a consonant at the beginning of a word`` () =
    translate "yellow" |> should equal "ellowyay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Y is treated like a vowel at the end of a consonant cluster`` () =
    translate "rhythm" |> should equal "ythmrhay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Y as second letter in two letter word`` () =
    translate "my" |> should equal "ymay"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``A whole phrase`` () =
    translate "quick fast run" |> should equal "ickquay astfay unray"


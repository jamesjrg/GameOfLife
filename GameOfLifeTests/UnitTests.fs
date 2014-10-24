module GameOfLifeTests

open NUnit.Framework
open GameOfLife.Game

[<Test>]
let ``neighbours for (0,0)`` ()=
    let expected = [
        (-1, -1);
        (-1, 0);
        (-1, 1);
        (0, -1);
        (0, 1);
        (1, -1);
        (1, 0);
        (1, 1);
    ]

    CollectionAssert.AreEquivalent (expected, getNeighbours (0, 0))

let splitLines (s:string) = 
    List.ofSeq(s.Split([|'\n'|]))

(* this function somewhat based on http://rosettacode.org/wiki/Conway%27s_Game_of_Life/Scala *)
let coordinatesFromAsciiArt art =
    let lines = splitLines art |> List.map (fun line -> line.TrimStart().Substring(1))
    seq {
        for (row, line) in List.mapi (fun row line -> (row, line)) lines do
        for (column, character) in Seq.mapi (fun column character -> (column, character)) line do
        if character = 'X' then yield (row, column)
    }

[<TestCase(
    "|XX
     |XX",
    "|XX
     |XX",
    "block, still life"
)>]
[<TestCase(
    "| XX 
     |X  X
     | XX ",
    "| XX 
     |X  X
     | XX ",
    "beehive, still life"
)>]
[<TestCase(
    "|   
     |XXX",
    "| X
     | X
     | X",
    "blinker, oscillator"
)>]
[<TestCase(
    "|
     | XXX
     |XXX",
    "|  X
     |X  X
     |X  X
     | X",
    "toad, oscillator"
)>]

let tick_tests initial expected message =
    Assert.AreEqual (coordinatesFromAsciiArt expected,
        tick (coordinatesFromAsciiArt initial),
        message)
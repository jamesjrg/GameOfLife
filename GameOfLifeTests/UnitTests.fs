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

let coordinatesFromAsciiArt art =
    [ (0,0); ]

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
     | XX",
    "| XX
     |X  X
     | XX",
    "beehive, still life"
)>]
let tick_tests initial expected message =
    Assert.AreEqual (coordinatesFromAsciiArt expected,
        coordinatesFromAsciiArt (tick (coordinatesFromAsciiArt initial)),
        message)
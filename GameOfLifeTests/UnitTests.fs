namespace GameOfLifeTests

open NUnit.Framework
open FsUnit

[<TestFixture>] 
type ``Given something or other`` ()=
    let lightBulb = new LightBulb(true)

    [<Test>] member x.
     ``when I ask whether it is On it answers true.`` ()=
            lightBulb.On |> should be True

    [<Test>] member x.
     ``when I convert it to a string it becomes "On".`` ()=
            string lightBulb |> should equal "On"
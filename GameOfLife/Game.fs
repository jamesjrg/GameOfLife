module GameOfLife.Game

open System.Collections.Generic

let getNeighbours (row, column) = 
    seq {
        for neighbourRow in row - 1 .. row + 1 do
        for neighbourColumn in column - 1 .. column + 1 do
        if not (neighbourRow = row && neighbourColumn = column) then yield (neighbourRow, neighbourColumn)
    }

let countLiveNeighbours cell oldCellsMemoized = 
   getNeighbours cell
   |> Seq.filter (fun neighbour -> oldCellsMemoized neighbour)
   |> Seq.length

(* following two functions somewhat based on https://github.com/chrisphelps/funConway/blob/master/src/test/scala/FunConway.scala *)
let shouldLive (cell : int * int) (oldCellsMemoized : (int * int) -> bool) =
    match (countLiveNeighbours cell oldCellsMemoized, oldCellsMemoized cell) with
    | (neighbourCount, true) when neighbourCount < 2 -> false
    | (neighbourCount, true) when neighbourCount > 3 -> false
    | (neighbourCount, true) -> true
    | (neighbourCount, false) when neighbourCount = 3 -> true
    | _ -> false

let candidateCells (oldCells : seq<int * int>) =
     oldCells
     |> Seq.collect getNeighbours 
     |> Seq.append oldCells
     |> set
     
let memoize f =
    let cache = Dictionary<_, _>()
    fun x ->
        let ok, res = cache.TryGetValue(x)
        if ok then res
        else
            let res = f x
            cache.[x] <- res
            res

let tick (oldCells : seq<int * int>) = 
    let oldCellsMemoized = memoize (fun candidate -> Seq.exists (fun elem -> elem = candidate) oldCells)
    oldCells
    |> candidateCells
    |> Set.filter (fun cell -> shouldLive cell oldCellsMemoized)


module GameOfLife.Grid

(* somewhat based on https://github.com/chrisphelps/funConway/blob/master/src/test/scala/FunConway.scala *)

open System.Collections.Generic

let getNeighbours row column = 
    seq { for neighbourRow in row - 1 .. row + 1 do
        yield! seq { for neighbourColumn in column - 1 .. column + 1 do
            if not (neighbourRow = row && neighbourColumn = column) then yield (neighbourRow, neighbourColumn) }
    }

let countLiveNeighbours cell oldCells = 
   1

let shouldLive cell isAlive oldCells =
    match (countLiveNeighbours cell oldCells, isAlive) with
    | (neighbourCount, true) when neighbourCount < 2 -> false
    | (neighbourCount, true) when neighbourCount > 3 -> false
    | (neighbourCount, true) -> true
    | (neighbourCount, false) when neighbourCount = 3 -> true
    | _ -> false

let candidateCells oldCells =
     oldCells
     |> List.collect getNeighbours 
     |> List.append oldCells
     |> set

let cellsWithLiveStatus candidateCells oldGridMemoized =
    List.map candidateCells (fun cell -> (cell, oldGridMemoized cell))

let memoize f =
    let cache = Dictionary<_, _>()
    fun x ->
        let ok, res = cache.TryGetValue(x)
        if ok then res
        else
            let res = f x
            cache.[x] <- res
            res

let tick oldGrid = 
    let oldGridMemoized = memoize (fun candidate -> Seq.exists oldGrid candidate)
    oldGrid
    |> candidateCells
    |> cellsWithLiveStatus oldGridMemoized
    |> Seq.filter shouldLive oldGridMemoized


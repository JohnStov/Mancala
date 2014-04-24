module Mancala.Tests

open Xunit
open Mancala

[<Fact>]
let BoardHasTwoSides() =
    let state = CreateBoard(0)
    Assert.Equal(2, state.Sides.Length)

[<Fact>]
let SideOneHasSevenPits() =
    let state = CreateBoard(0)
    Assert.Equal(7, state.Sides.[0].Length)

[<Fact>]
let SideTwoHasSevenPits() =
    let state = CreateBoard(0)
    Assert.Equal(7, state.Sides.[1].Length)

[<Fact>]
let BoardCanBeInitialisedWith3StonesPerPit() =
    let state = CreateBoard(3)
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], state.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], state.Sides.[1])

[<Fact>]
let PlayerCannotPlayInvalidPlayer() =
    let state = CreateBoard(3)
    let allowed = CanPlay state 2 0 
    Assert.False(allowed)

[<Fact>]
let PlayerCannotPlayInvalidPit() =
    let state = CreateBoard(3)
    let allowed = CanPlay state 1 7 
    Assert.False(allowed)

[<Fact>]
let PlayerCannotPlayEmptyPit() =
    let state = CreateBoard(0)
    let allowed = CanPlay state 1 5 
    Assert.False(allowed)

[<Fact>]
let PlayerCanPlayEmptyPit() =
    let state = CreateBoard(3)
    let allowed = CanPlay state 1 5 
    Assert.True(allowed)

[<Fact>]
let BoardWithNonEmptyPitsIsNotFinished() =
    let state = CreateBoard(3)
    let finished = IsFinished state
    Assert.False(finished)

[<Fact>]
let BoardWithEmptyPitsOnOneSideIsFinished() =
    let state = CreateBoard(0)
    let finished = IsFinished state
    Assert.True(finished)

[<Fact>]
let PlayRedistributesStonesOnPlayersSide() =
    let state = CreateBoard(3)
    let newState = Play state 0 6
    Assert.Equal<int[]> ([|0;3;3;4;4;4;0|], newState.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], newState.Sides.[1])

[<Fact>]
let PlayRedistributesStonesIntoScoring() =
    let state = CreateBoard(3)
    let newState = Play state 0 3
    Assert.Equal<int[]> ([|1;4;4;0;3;3;3|], newState.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], newState.Sides.[1])

[<Fact>]
let PlayRedistributesStonesIntoOpponentsSide() =
    let state = CreateBoard(3)
    let newState = Play state 0 1
    Assert.Equal<int[]> ([|1;0;3;3;3;3;3|], newState.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;4;4|], newState.Sides.[1])

[<Fact>]
let PlayRedistributesStonesIntoBothSides() =
    let state = CreateBoard(3)
    let newState = Play state 0 2
    Assert.Equal<int[]> ([|1;4;0;3;3;3;3|], newState.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;4|], newState.Sides.[1])

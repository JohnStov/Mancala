module Mancala.Tests

open Xunit
open Mancala

[<Fact>]
let BoardHasTwoSides() =
    let state = new BoardState(0)
    Assert.Equal(2, state.Sides.Length)

[<Fact>]
let SideOneHasSevenPits() =
    let state = new BoardState(0)
    Assert.Equal(7, state.Sides.[0].Length)

[<Fact>]
let SideTwoHasSevenPits() =
    let state = new BoardState(0)
    Assert.Equal(7, state.Sides.[1].Length)

[<Fact>]
let BoardCanBeInitialisedWith3StonesPerPit() =
    let state = new BoardState(3)
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], state.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], state.Sides.[1])

[<Fact>]
let PlayerCannotPlayInvalidPayer() =
    let state = new BoardState(3)
    let allowed = CanPlay state 2 0 
    Assert.False(allowed)

[<Fact>]
let PlayerCannotPlayInvalidPit() =
    let state = new BoardState(3)
    let allowed = CanPlay state 1 7 
    Assert.False(allowed)

[<Fact>]
let PlayerCannotPlayEmptyPit() =
    let state = new BoardState(0)
    let allowed = CanPlay state 1 5 
    Assert.False(allowed)

[<Fact>]
let PlayerCanPlayEmptyPit() =
    let state = new BoardState(3)
    let allowed = CanPlay state 1 5 
    Assert.True(allowed)

[<Fact>]
let BoardWithNonEmptyPitsIsNotFinished() =
    let state = new BoardState(3)
    let finished = IsFinished state
    Assert.False(finished)

[<Fact>]
let BoardWithEmptyPitsOnOneSideIsFinished() =
    let state = new BoardState(0)
    let finished = IsFinished state
    Assert.True(finished)

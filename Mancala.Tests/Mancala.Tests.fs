module Mancala.Tests

open Xunit
open Mancala

[<Fact>]
let ``Board Has Two Sides``() =
    let state = CreateBoard(0)
    Assert.Equal(2, state.Sides.Length)

[<Fact>]
let ``Side One Has Seven Pits``() =
    let state = CreateBoard(0)
    Assert.Equal(7, state.Sides.[0].Length)

[<Fact>]
let ``Side Two Has Seven Pits``() =
    let state = CreateBoard(0)
    Assert.Equal(7, state.Sides.[1].Length)

[<Fact>]
let ``Board Can Be Initialised With 3 Stones Per Pit``() =
    let state = CreateBoard(3)
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], state.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], state.Sides.[1])

[<Fact>]
let ``Player Cannot Play Invalid Side``() =
    let state = CreateBoard(3)
    let allowed = CanPlay state 2 0 
    Assert.False(allowed)

[<Fact>]
let ``Player Cannot Play Invalid Pit``() =
    let state = CreateBoard(3)
    let allowed = CanPlay state 1 7 
    Assert.False(allowed)

[<Fact>]
let ``Player Cannot Play Empty Pit``() =
    let state = CreateBoard(0)
    let allowed = CanPlay state 1 5 
    Assert.False(allowed)

[<Fact>]
let ``Player Can Play Non-Empty Pit``() =
    let state = CreateBoard(3)
    let allowed = CanPlay state 1 5 
    Assert.True(allowed)

[<Fact>]
let ``Board With Non-Empty Pits On Both Sides Is Not Finished``() =
    let state = CreateBoard(3)
    let finished = IsFinished state
    Assert.False(finished)

[<Fact>]
let ``Board With Empty Pits On One Side Is Finished``() =
    let state = CreateBoard(0)
    let finished = IsFinished state
    Assert.True(finished)

[<Fact>]
let ``Play Redistributes Stones On Player's Side``() =
    let state = CreateBoard(3)
    let newState, _ = Play state 0 6
    Assert.Equal<int[]> ([|0;3;3;4;4;4;0|], newState.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], newState.Sides.[1])

[<Fact>]
let ``Play Redistributes Stones Into Store``() =
    let state = CreateBoard(3)
    let newState, _ = Play state 0 3
    Assert.Equal<int[]> ([|1;4;4;0;3;3;3|], newState.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;3|], newState.Sides.[1])

[<Fact>]
let ``Play Redistributes Stones Into Opponents Side``() =
    let state = CreateBoard(3)
    let newState, _ = Play state 0 1
    Assert.Equal<int[]> ([|1;0;3;3;3;3;3|], newState.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;4;4|], newState.Sides.[1])

[<Fact>]
let ``Play Redistributes Stones Into Both Sides``() =
    let state = CreateBoard(3)
    let newState, _ = Play state 0 2
    Assert.Equal<int[]> ([|1;4;0;3;3;3;3|], newState.Sides.[0])
    Assert.Equal<int[]> ([|0;3;3;3;3;3;4|], newState.Sides.[1])

[<Fact>]
let ``Play That Does Not End At Store Switches Players``() =
    let state = CreateBoard(3)
    let _, nextPlayer = Play state 0 4
    Assert.Equal (1, nextPlayer)

[<Fact>]
let ``Play That Ends At Store Gives Player Another Turn``() =
    let state = CreateBoard(3)
    let _, nextPlayer = Play state 0 3
    Assert.Equal (0, nextPlayer)

[<Fact>]
let ``Automove picks smallest pit that will give another go``() =
    let state = new BoardState([|0;0;0;3;4;0;0|], [|0;3;3;3;3;3;3;3|])
    let move = Mancala.AutoMove state 0
    Assert.Equal (3, move)

[<Fact>]
let ``If no second turn available Automove picks biggest pile``() =
    let state = new BoardState([|0;0;3;4;0;0;0|], [|0;3;3;3;3;3;3;3|])
    let move = Mancala.AutoMove state 0
    Assert.Equal (3, move)

[<Fact>]
let ``AutoMove tries to prevent opponent getting second move``() =
    let state = new BoardState([|0;3;1;0;0;0;0|], [|0;0;0;0;0;4;1|])
    let move = Mancala.AutoMove state 0
    Assert.Equal (2, move)

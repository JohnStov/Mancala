module Mancala.Tests

open Xunit
open Mancala

[<Fact>]
let ``Board Has Two Sides``() =
    let board = CreateBoard(0)
    Assert.Equal(2, board.Length)

[<Fact>]
let ``Side One Has Seven Pits``() =
    let board = CreateBoard(0)
    Assert.Equal(7, board.[0].Length)

[<Fact>]
let ``Side Two Has Seven Pits``() =
    let board = CreateBoard(0)
    Assert.Equal(7, board.[1].Length)

[<Fact>]
let ``Board Can Be Initialised With 3 Stones Per Pit``() =
    let board = CreateBoard(3)
    Assert.True ([0;3;3;3;3;3;3] = board.[0])
    Assert.True ([0;3;3;3;3;3;3] = board.[1])

[<Fact>]
let ``Player Cannot Play Invalid Side``() =
    let board = CreateBoard(3)
    let allowed = CanPlay board 2 0 
    Assert.False(allowed)

[<Fact>]
let ``Player Cannot Play Invalid Pit``() =
    let board = CreateBoard(3)
    let allowed = CanPlay board 1 7 
    Assert.False(allowed)

[<Fact>]
let ``Player Cannot Play Empty Pit``() =
    let board = CreateBoard(0)
    let allowed = CanPlay board 1 5 
    Assert.False(allowed)

[<Fact>]
let ``Player Can Play Non-Empty Pit``() =
    let board = CreateBoard(3)
    let allowed = CanPlay board 1 5 
    Assert.True(allowed)

[<Fact>]
let ``Board With Non-Empty Pits On Both Sides Is Not Finished``() =
    let board = CreateBoard(3)
    let finished = IsFinished board
    Assert.False(finished)

[<Fact>]
let ``Board With Empty Pits On One Side Is Finished``() =
    let board = CreateBoard(0)
    let finished = IsFinished board
    Assert.True(finished)

[<Fact>]
let ``Play Redistributes Stones On Player's Side``() =
    let board = CreateBoard(3)
    let newboard, _ = Play board 0 6
    Assert.True ([0;3;3;4;4;4;0] = newboard.[0])
    Assert.True ([0;3;3;3;3;3;3] = newboard.[1])

[<Fact>]
let ``Play Redistributes Stones Into Store``() =
    let board = CreateBoard(3)
    let newboard, _ = Play board 0 3
    Assert.True ([1;4;4;0;3;3;3] = newboard.[0])
    Assert.True ([0;3;3;3;3;3;3] = newboard.[1])

[<Fact>]
let ``Play Redistributes Stones Into Opponents Side``() =
    let board = CreateBoard(3)
    let newboard, _ = Play board 0 1
    Assert.True ([1;0;3;3;3;3;3] = newboard.[0])
    Assert.True ([0;3;3;3;3;4;4] = newboard.[1])

[<Fact>]
let ``Play Redistributes Stones Into Both Sides``() =
    let board = CreateBoard(3)
    let newboard, _ = Play board 0 2
    Assert.True ([1;4;0;3;3;3;3] = newboard.[0])
    Assert.True ([0;3;3;3;3;3;4] = newboard.[1])

[<Fact>]
let ``Play That Does Not End At Store Switches Players``() =
    let board = CreateBoard(3)
    let _, nextPlayer = Play board 0 4
    Assert.Equal (1, nextPlayer)

[<Fact>]
let ``Play That Ends At Store Gives Player Another Turn``() =
    let board = CreateBoard(3)
    let _, nextPlayer = Play board 0 3
    Assert.Equal (0, nextPlayer)

[<Fact>]
let ``Automove picks smallest pit that will give another go``() =
    let board = [[0;0;0;3;4;0;0]; [0;3;3;3;3;3;3;3]]
    let move = Mancala.AutoMove board 0
    Assert.Equal (3, move)

[<Fact>]
let ``If no second turn available Automove picks biggest pile``() =
    let board = [[0;0;3;4;0;0;0]; [0;3;3;3;3;3;3;3]]
    let move = Mancala.AutoMove board 0
    Assert.Equal (3, move)

[<Fact>]
let ``AutoMove tries to prevent opponent getting second move``() =
    let board = [[0;3;1;0;0;0;0]; [0;0;0;0;0;4;1]]
    let move = Mancala.AutoMove board 0
    Assert.Equal (2, move)

[<Fact>]
let ``Player 1 has won``() =
    let board = [[10;3;1;0;0;0;0]; [9;0;0;0;0;0;0]]
    let winner = Mancala.Winner board
    Assert.Equal (0, winner.Value)

[<Fact>]
let ``Game is drawn``() =
    let board = [[10;3;1;0;0;0;0]; [10;0;0;0;0;0;0]]
    let winner = Mancala.Winner board
    Assert.True (winner.IsNone)

[<Fact>]
let ``No winner if game is not finished``() =
    let board = [[10;3;1;0;0;0;0]; [8;1;0;0;0;0;0]]
    let winner = Mancala.Winner board
    Assert.True (winner.IsNone)

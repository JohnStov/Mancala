// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open Mancala

let DisplayBoard (board : BoardState) player =
    let top = player
    let bottom = Mancala.OtherPlayer player
    printf "\nPlayer %d : (%d) %d %d %d %d %d %d\n" (top + 1) board.Sides.[top].[0] board.Sides.[top].[1] board.Sides.[top].[2] board.Sides.[top].[3] board.Sides.[top].[4] board.Sides.[top].[5] board.Sides.[top].[6]
    printf "Player %d :     %d %d %d %d %d %d (%d)\n\n"  (bottom + 1) board.Sides.[bottom].[6] board.Sides.[bottom].[5] board.Sides.[bottom].[4] board.Sides.[bottom].[3] board.Sides.[bottom].[2] board.Sides.[bottom].[1] board.Sides.[bottom].[0]

let GetMove player =
    printf "Player %d - Select Move (1-6):" (player+1)
    let mutable input = '0'
    while input < '1' || input > '6' do
        input <- char (Console.Read())
    (int input) - (int '0')
        
[<EntryPoint>]
let main argv = 
    let mutable player = 0
    let mutable board = Mancala.CreateBoard 3
    DisplayBoard board player
    while (not (Mancala.IsFinished board)) do
        let move = GetMove player
        let newboard, newplayer = Mancala.Play board player move
        board <- newboard
        player <- newplayer
        DisplayBoard board player
    0 // return an integer exit code

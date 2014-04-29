// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open Mancala

let DisplayBoard (board : int list list) player =
    let top = player
    let bottom = Mancala.OtherPlayer player
    printf "\nPlayer %d : (%d) %d %d %d %d %d %d\n" (top + 1) board.[top].[0] board.[top].[1] board.[top].[2] board.[top].[3] board.[top].[4] board.[top].[5] board.[top].[6]
    printf "Player %d :     %d %d %d %d %d %d (%d)\n\n"  (bottom + 1) board.[bottom].[6] board.[bottom].[5] board.[bottom].[4] board.[bottom].[3] board.[bottom].[2] board.[bottom].[1] board.[bottom].[0]

let GetMove player =
    printf "Player %d - Select Move (1-6):" (player+1)
    let mutable input = '0'
    while input < '1' || input > '6' do
        input <- char (Console.Read())
    (int input) - (int '0')
        
let DisplayMove player move =
    printf "Player %d moved %d\n" (player+1) move

let DisplayWinner player = 
    match player with
    | None -> printf "\nA Draw!\n"
    | Some(x) -> printf "\nThe winner was Player %d\n" (x+1)

    ignore (Console.ReadLine())
        
[<EntryPoint>]
let main argv = 
    let mutable player = 0
    let mutable board = Mancala.CreateBoard 3
    DisplayBoard board player
    while (not (Mancala.IsFinished board)) do
        let move = 
            if (player = 0) 
            then GetMove player
            else Mancala.AutoMove board player
        DisplayMove player move
        let newboard, newplayer = Mancala.Play board player move
        board <- newboard
        player <- newplayer
        DisplayBoard board player
    DisplayWinner (Mancala.Winner board)
    0 // return an integer exit code

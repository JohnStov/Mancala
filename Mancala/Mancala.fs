module Mancala

type BoardState(stonesPerPit) = 
    let CreateSide(stonesPerPit : int) =
        [| 0; stonesPerPit; stonesPerPit; stonesPerPit; stonesPerPit; stonesPerPit; stonesPerPit |]

    member x.Sides = [| CreateSide(stonesPerPit); CreateSide(stonesPerPit) |]

let CanPlay (state : BoardState) player pit =
    match player with
    | 0 | 1 -> 
        match pit with 
        | 1 | 2 | 3 | 4 | 5 | 6 ->
            state.Sides.[player].[pit] > 0
        | _ -> false
    | _ -> false

let IsFinished(state : BoardState) = 
    let finished (side : int array) = 
        side.[1..6] = [|0;0;0;0;0;0|]

    finished state.Sides.[0] || finished state.Sides.[1]
    

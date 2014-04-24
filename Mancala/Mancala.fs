module Mancala

type BoardState(side1, side2) = 
    member x.Sides = [| side1; side2 |]

let CreateBoard(stonesPerPit : int) =
    let CreateSide(stonesPerPit : int) = 
        Array.init 7 (fun i -> if i = 0 then 0 else stonesPerPit)

    new BoardState(CreateSide(stonesPerPit), CreateSide(stonesPerPit))

let CanPlay (state : BoardState) player pit =
        if (player < state.Sides.Length && pit < state.Sides.[0].Length)
        then state.Sides.[player].[pit] > 0
        else false 

let IsFinished(state : BoardState) = 
    let finished (side : int array) = 
        side.[1..6] |> Array.sum = 0
    finished state.Sides.[0] || finished state.Sides.[1]

let IncrementPit (state : BoardState) player pit =
    let Increment (side : int array) pit =
        Array.append side.[0..(pit-1)] (Array.append [|(side.[pit] + 1)|] side.[(pit+1)..])
    match player with
    | 0 -> new BoardState(Increment state.Sides.[0] pit, state.Sides.[1])
    | _ -> new BoardState(state.Sides.[0], Increment state.Sides.[1] pit)

    
let EmptyPit (state : BoardState) player pit =
    let Empty (side : int array) pit =
        Array.append side.[0..(pit-1)] (Array.append [|0|] side.[(pit+1)..])
    match player with
    | 0 -> new BoardState(Empty state.Sides.[0] pit, state.Sides.[1])
    | _ -> new BoardState(state.Sides.[0], Empty state.Sides.[1] pit)
    
let NextPlayerAndPit player pit =
    let OtherPlayer player =
        match player with
        | 0 -> 1
        | _ -> 0
    match pit with
    | 0 -> OtherPlayer player, 6
    | _ -> player, (pit - 1)

let rec Distribute (state : BoardState) player pit count =
    match count with
    | 0 -> state
    | 1 -> IncrementPit state player pit
    | _ -> 
        let nextPlayer, nextPit = NextPlayerAndPit player pit  
        Distribute (IncrementPit state player pit) nextPlayer nextPit (count-1)

let Redistribute (state : BoardState) player pit =
    let count = state.Sides.[player].[pit]
    let nextPlayer, nextPit = NextPlayerAndPit player pit
    Distribute (EmptyPit state player pit) nextPlayer nextPit count

let Play (state : BoardState) player pit =
    match CanPlay state player pit with
    | false -> state
    | true -> Redistribute state player pit 
    

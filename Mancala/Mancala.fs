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

let IncrementPit (state : BoardState) side pit =
    let Increment (side : int array) pit =
        Array.append side.[0..(pit-1)] (Array.append [|(side.[pit] + 1)|] side.[(pit+1)..])
    match side with
    | 0 -> new BoardState(Increment state.Sides.[0] pit, state.Sides.[1])
    | _ -> new BoardState(state.Sides.[0], Increment state.Sides.[1] pit)

    
let EmptyPit (state : BoardState) side pit =
    let Empty (side : int array) pit =
        Array.append side.[0..(pit-1)] (Array.append [|0|] side.[(pit+1)..])
    match side with
    | 0 -> new BoardState(Empty state.Sides.[0] pit, state.Sides.[1])
    | _ -> new BoardState(state.Sides.[0], Empty state.Sides.[1] pit)
    
let OtherPlayer side =
    match side with
    | 0 -> 1
    | _ -> 0

let NextSideAndPit side pit =
    match pit with
    | 0 -> OtherPlayer side, 6
    | _ -> side, (pit - 1)

let rec Distribute (state : BoardState) side pit count =
    match count with
    | 0 -> state
    | 1 -> IncrementPit state side pit
    | _ -> 
        let nextSide, nextPit = NextSideAndPit side pit  
        Distribute (IncrementPit state side pit) nextSide nextPit (count-1)

let Redistribute (state : BoardState) side pit =
    let count = state.Sides.[side].[pit]
    let nextPlayer = if count = pit then side else OtherPlayer side
    let nextSide, nextPit = NextSideAndPit side pit
    (Distribute (EmptyPit state side pit) nextSide nextPit count), nextPlayer

let Play (state : BoardState) player pit =
    match CanPlay state player pit with
    | false -> state, player
    | true -> Redistribute state player pit 


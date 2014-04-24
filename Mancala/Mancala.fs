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

let IncrementPit (side : int array) pit =
    Array.append side.[0..(pit-1)] (Array.append [|(side.[pit] + 1)|] side.[(pit+1)..])

let EmptyPit (side : int array) pit =
    Array.append side.[0..(pit-1)] (Array.append [|0|] side.[(pit+1)..])
    
let rec Distribute side pit count =
    match count with
    | 0 -> side
    | 1 -> IncrementPit side pit
    | _ -> Distribute (IncrementPit side pit) (pit-1) (count-1)

let Redistribute (side : int array) pit =
    let count = side.[pit]
    Distribute (EmptyPit side pit) (pit-1) count


let Play (state : BoardState) player pit =
    match CanPlay state player pit with
    | false -> state
    | true -> 
        match player with
        | 0 -> new BoardState ((Redistribute state.Sides.[0] pit), state.Sides.[1])
        | 1 -> new BoardState (state.Sides.[0], (Redistribute state.Sides.[1] pit))
        | _ -> state
    

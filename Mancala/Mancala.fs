module Mancala

type BoardState(side1, side2) = 
    member x.Sides = [side1; side2]

let CreateBoard stonesPerPit =
    let CreateSide stonesPerPit = 
        List.init 7 (fun i -> if i = 0 then 0 else stonesPerPit)
    new BoardState(CreateSide stonesPerPit, CreateSide stonesPerPit)

let CanPlay (state : BoardState) player pit =
        if (player < state.Sides.Length && pit < state.Sides.[0].Length)
        then state.Sides.[player].[pit] > 0
        else false 

let IsFinished(state : BoardState) = 
    let finished side = 
        List.tail side |> List.sum = 0
    finished state.Sides.[0] || finished state.Sides.[1]

let IncrementPit (state : BoardState) side pit =
    let rec Increment side pit =
        match side, pit with
        | [], _ -> []
        | h::t, 0 -> (h+1) :: t
        | h::t, n -> h :: Increment t (n-1)
    match side with
    | 0 -> new BoardState(Increment state.Sides.[0] pit, state.Sides.[1])
    | _ -> new BoardState(state.Sides.[0], Increment state.Sides.[1] pit)

    
let EmptyPit (state : BoardState) side pit =
    let rec Empty side pit =
        match side, pit with
        | [], _ -> []
        | h::t, 0 -> 0 :: t
        | h::t, n -> h :: Empty t (n-1)
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

let TwoMovePlay side =
    let pits = List.tail side
    let secondMovePits = List.mapi (fun i x -> if x = i + 1 then x else 0) pits
    try
        Some(List.find (fun x -> x > 0) secondMovePits)
    with
    | :? System.Collections.Generic.KeyNotFoundException -> None

let BestPlay side =
    let pits = List.tail side
    (List.findIndex (fun x -> x = List.max pits) pits) + 1

let rec BestMove side =
    match TwoMovePlay side with
    | Some(x) -> x
    | None -> BestPlay side

let rec AutoMoveRec (state : BoardState) player best =       
    let side = state.Sides.[player]

    match side, best with
    | [_;0;0;0;0;0;0], None -> 6
    | [_;0;0;0;0;0;0], Some(x) -> x
    | _, _ ->
        let move = BestMove side

        let outcome, _ = Play state player move
        let otherSide = outcome.Sides.[OtherPlayer player]
        match TwoMovePlay otherSide with
        | None -> move
        | Some(_) -> 
            let newBest = 
                match best with
                | None -> move
                | Some(x) -> x

            let newState = EmptyPit state player move
            AutoMoveRec newState player (Some(newBest))

let AutoMove (state : BoardState) player = 
    AutoMoveRec state player None

let Winner (state : BoardState) =
    match IsFinished state with
    | false -> None
    | true -> 
        if (state.Sides.[0].[0] > state.Sides.[1].[0]) then Some(0)
        elif (state.Sides.[0].[0] < state.Sides.[1].[0]) then Some(1)
        else None
        



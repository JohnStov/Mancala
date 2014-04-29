module Mancala

let CreateBoard stonesPerPit =
    let CreateSide stonesPerPit = 
        List.init 7 (fun i -> if i = 0 then 0 else stonesPerPit)
    [CreateSide stonesPerPit; CreateSide stonesPerPit]

let CanPlay (board : int list list) player pit =
        if (player < board.Length && pit < board.[0].Length)
        then board.[player].[pit] > 0
        else false 

let IsFinished (board : int list list) = 
    let finished side = 
        List.tail side |> List.sum = 0
    finished board.[0] || finished board.[1]

let IncrementPit (board : int list list) playerSide pit =
    let rec Increment side pit =
        match side, pit with
        | [], _ -> []
        | h::t, 0 -> (h+1) :: t
        | h::t, n -> h :: Increment t (n-1)
    match playerSide with
    | 0 -> [Increment board.[0] pit; board.[1]]
    | _ -> [board.[0]; Increment board.[1] pit]

    
let EmptyPit (board : int list list) playerSide pit =
    let rec Empty side pit =
        match side, pit with
        | [], _ -> []
        | h::t, 0 -> 0 :: t
        | h::t, n -> h :: Empty t (n-1)
    match playerSide with
    | 0 -> [Empty board.[0] pit; board.[1]]
    | _ -> [board.[0]; Empty board.[1] pit]
    
let OtherPlayer side =
    match side with
    | 0 -> 1
    | _ -> 0

let NextSideAndPit side pit =
    match pit with
    | 0 -> OtherPlayer side, 6
    | _ -> side, (pit - 1)

let rec Distribute board side pit count =
    match count with
    | 0 -> board
    | 1 -> IncrementPit board side pit
    | _ -> 
        let nextSide, nextPit = NextSideAndPit side pit  
        Distribute (IncrementPit board side pit) nextSide nextPit (count-1)

let Redistribute (board : int list list) side pit =
    let count = board.[side].[pit]
    let nextPlayer = if count = pit then side else OtherPlayer side
    let nextSide, nextPit = NextSideAndPit side pit
    (Distribute (EmptyPit board side pit) nextSide nextPit count), nextPlayer

let Play board player pit =
    match CanPlay board player pit with
    | false -> board, player
    | true -> Redistribute board player pit 

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

let AutoMove board player = 
    let rec AutoMoveRec (board : int list list) player best =       
        let side = board.[player]

        match side, best with
        | [_;0;0;0;0;0;0], None -> 6
        | [_;0;0;0;0;0;0], Some(x) -> x
        | _, _ ->
            let move = BestMove side

            let outcome, _ = Play board player move
            let otherSide = outcome.[OtherPlayer player]
            match TwoMovePlay otherSide with
            | None -> move
            | Some(_) -> 
                let newBest = 
                    match best with
                    | None -> move
                    | Some(x) -> x

                let newboard = EmptyPit board player move
                AutoMoveRec newboard player (Some(newBest))

    AutoMoveRec board player None

let Winner board =
    match IsFinished board with
    | false -> None
    | true -> 
        if (board.[0].[0] > board.[1].[0]) then Some(0)
        elif (board.[0].[0] < board.[1].[0]) then Some(1)
        else None
        



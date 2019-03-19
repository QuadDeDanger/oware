module Oware

type StartingPosition =
    | South
    | North

type GameState =
    | South's_turn
    | North's_turn
    | Game_Ended_in_a_draw
    | South_won
    | North_won

type Player = {
 houses_number : int*int*int*int*int*int ; StoreCount : int
}
 
type Board = {
 player1: Player; player2: Player ; gameState: GameState 
}

//let getSeeds n board = 
 //let hasSeed = Board.house1=1; 

let useHouse n board = failwith "Not implemented"

let start position = failwith "Not implemented"

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code

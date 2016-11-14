module SimpleTour


// brute force attack on Knight's Tour problem
//
// Approach 1:
//
//     Pick a random location on the board
//     begin recursion
//          Get a list of valid moves from the current location
//          pick a random move
//          make the move
//          if no more moves return
//          otherwise recurse

open Rop
open GraphLib

type Solution<'T> =
    | Valid of 'T
    | Incomplete of 'T
    | Invalid of 'T

let isValid m = Valid m
let isInvalid m = Invalid m
let isIncomplete m = Incomplete m

type Move = Move of (int * int)
type Position = Position of (int * int)
type MoveList = MoveList of Move list
type TryList = TryList of Move list
type ValidMoveList = ValidMoveList of Move list
type Offset = Offset of (int * int)

let positionToMove (Move move) =
    Position move

let moveToPosition (Position pos) =
    Move pos

type State = {
    BoardSize: int;
    CurrentPosition: Position;
    CurrentBoard: MoveList;
    TriedMoves: TryList;
    ValidMoves: ValidMoveList;
}




// the offset positions for the knight
let offsets =   [Offset(1,-2);Offset(2,-1);
                 Offset(2,1);Offset(1,2);
                 Offset(2,-1);Offset(1,-2);
                 Offset(-1,-2);Offset(-2,-1)]

let rand = new System.Random((int)System.DateTime.Now.Ticks)
let randN max = rand.Next max
let randPoint () = (randN 8, randN 8)

// returns the valid moves given a move, then board, and a list of tried positions
let validMoves state =
    let { BoardSize = size; CurrentPosition = Position(lx,ly); CurrentBoard = (MoveList moves); TriedMoves = (TryList tries)} = state
    {
        state with ValidMoves = offsets 
                |> List.map (fun (Offset (x,y)) -> Move (lx + x, ly + y))        
                |> List.filter (fun (Move (x,y)) -> x >= 0 && y >= 0 && x < size && y < size )
                |> List.filter (fun move -> not (List.exists ((=) move) moves))        
                |> List.filter (fun move -> not (List.exists ((=) move) tries)) |> ValidMoveList
    }

// rule checker, the board must contain 64 moves with no duplicates
let checkResult state = 
    let {CurrentBoard = (MoveList moves) } = state
    if (List.length moves = 0 || List.length moves <> 64) then moves |> isIncomplete 
    else 
        let rec inner rest =
            match rest with
            | head :: tail -> if not (List.exists ((=) head) tail)
                              then inner tail
                              else moves |>  isInvalid
            | [] -> moves |> isValid
        inner moves


let closestToCenter state =
        let distanceToCenter size (Move (x,y)) =
            let dx = (size / 2) - x
            let dy = (size / 2) - y
            (dx*dx, dy*dy)
        let {BoardSize = size; ValidMoves = (ValidMoveList moves)} = state
        let sorted =
            moves 
            |> List.map (fun move -> (move, distanceToCenter size move)) 
            |> List.sortWith (fun (_, d1) (_, d2) -> if d1 < d2 then -1 elif d1 > d2 then 1 else 0 ) 
        let (Move result, d) = List.head sorted
        Move result

let selectMove state = 
    validMoves state |> closestToCenter



(*
type State = {
    BoardSize: int;
    CurrentPosition: Position;
    CurrentBoard: MoveList;
    TriedMoves: TryList;
    ValidMoves: ValidMoveList;
}

*)
    
//let makeMove state =
//    let { BoardSize = size; CurrentPosition = Position(lx,ly); CurrentBoard = (MoveList moves); TriedMoves = (TryList tries)} = state
    



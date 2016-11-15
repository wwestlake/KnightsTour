(*
    Knights Tour in F#, an algorithm to solve the knights tour problem
    Copyright (C) 2016  William W. Westlake
    wwestlake@lagdaemon.com

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Source code available at: https://github.com/wwestlake/KnightsTour
*)

// #load "Rop.fs"

module SimpleTour
open Rop



type Position = Position of (int * int)
type Move = Move of (int * int)
type Board = Board of (int * int) list
type ValidMoves = ValidMoves of (int * int) list
type TriedMoves = TriedMoves of (int * int) list
type TriedMovesStack = TriedMovesStack of (int * int) list list

type State = {
    Size: int;
    Position: Position;
    Board: Board;
    ValidMoves: ValidMoves;
    TriedMoves: TriedMoves;
    TriedMovesStack: TriedMovesStack;
}

let createState size pos board valid tried triedStack =
    {
        Size = size;
        Position = (Position pos);
        Board = (Board board);
        ValidMoves = (ValidMoves valid);
        TriedMoves = (TriedMoves tried);
        TriedMovesStack = (TriedMovesStack triedStack);
    }



let startLocations size = [
        for i in [1..size] do
            for j in [1..size] do
                yield (Position (i,j))
    ]

let offsets = [Position ( 2,-1); Position ( 2,-1);  
               Position ( 2, 1); Position ( 1, 2);
               Position ( 2,-1); Position ( 1,-2);
               Position (-1,-2); Position (-2,-1)]


let validMoves (state: State) =
    let { Size = size; Position = (Position (x,y)); Board = (Board board); TriedMoves = (TriedMoves tried) } = state
    offsets |> List.map (fun (Position (ox,oy)) -> (x+ox, y+oy))
            |> List.filter (fun (x,y) -> x > 0 && y > 0 && x <= size && y <= size)
            |> List.filter (fun pos -> not (List.exists ((=) pos) board))
            |> List.filter (fun pos -> not (List.exists ((=) pos) tried))
            |> ValidMoves

let pickClosestToCenter (state: State) =
    let distance (x,y) =
        let dx = ((state.Size / 2) - x)
        let dy = ((state.Size / 2) - y)
        (dx * dx) + (dy * dy)
    match validMoves state with
    | ValidMoves list -> list |> List.map( fun (x,y) -> (x,y,distance (x,y)))
                              |> List.sortWith (fun (_,_,d1) (_,_,d2) -> d1.CompareTo(d2) )
                              |> List.head |> (fun (x,y,_) -> (x,y)) |> Move


let addTry (Move move) state =
    let { TriedMoves = (TriedMoves tries) } = state
    {
        state with TriedMoves = (move :: tries) |> TriedMoves
    }

let pushTries state =
    let { TriedMovesStack = (TriedMovesStack stack) } = state
    let{TriedMoves = tries} = state
    {
        state with TriedMovesStack = 
                            match tries with
                            | TriedMoves moves ->  (moves :: stack) |> TriedMovesStack
                   TriedMoves =  [] |> TriedMoves
    }

let popTries state =
    let { TriedMovesStack = (TriedMovesStack stack) } = state
    {
        state with TriedMovesStack = List.tail stack |> TriedMovesStack
                   TriedMoves = List.head stack |> TriedMoves
    }

let pushMove state (move: Move) =
    let { Board = (Board board) } = state
    {
        state with Board = 
                        match move with
                        | Move m -> (m :: board) |> Board
                   Position =
                        match move with
                        | Move m -> (Position m)
    }

let popMove state =
    let { Board = (Board board) } = state
    {
        state with Board =  (List.tail board) |> Board
                   Position = (List.head board) |> Position
    }

let backTrack state =
    state |> popTries |> popMove


let makeMove state =
    let pushMoveToState = pushMove state
    let move = pickClosestToCenter state
    let addTryToState = addTry move
    move |> pushMoveToState |> pushTries |> addTryToState
    
    



let test = createState 8 (4,4) [] [] [] []

pickClosestToCenter test





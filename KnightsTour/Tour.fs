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

module Tour

let isPowerOf2 x =
    ( not (x = 0) ) && (( x &&& (x - 1) = 0))


type NodeCondition =
    | Available
    | Taken
    


type Node = {
    Location: int * int;
    FromLinks: Node list;
    ToLinks: Node list;
    Condition: NodeCondition;
}

let newNode loc fromLinks toLinks condition =
    {
        Location = loc;
        FromLinks = fromLinks;
        ToLinks = toLinks;
        Condition = condition;
    }

let defaultNode =
    newNode (0,0) [] [] Available

let apppendFromLink node link =
    { node with FromLinks = link :: node.FromLinks }


let appendToLinks (node: Node) (link: Node) =
    { node with ToLinks = link :: node.ToLinks }




type Board(size: int) =

    let valid = 
        if isPowerOf2 size 
        then true 
        else failwith "Board size must be a power of 2"

    let mutable board = Array2D.init size size (fun i j -> 0)
    let mutable size = 0
    let mutable root = defaultNode
    let rand = new System.Random((int)System.DateTime.Now.Ticks)

    member this.Root 
        with get() = root
        and set(value:Node) = root <- value

    member this.StartLocation
        with get() = root.Location
        and set(value: int * int) = root <- { root with Location = value }

    member this.GameBoard
        with get() = board
        and set(value: int[,]) = 
                size <- value.GetLength( 0 )
                board <- value

    member this.Size 
        with get()  = size
        and set(value) = size <- value

    member this.validMoves (x,y) =
        [(x+1, y+2); (x+1,y-2); (x+2, y+1); (x+2,y-1);
            (x-1, y-2); (x-1,y+2); (x-2, y-1); (x-2,y+1);] 
            |> List.filter (fun (x,y) -> (x>=0) && (y>=0) && (x<size) && (y<size) 
                                        && (board.[x,y]=0))
    
    


    //member this.makeMove() : (int * int) =
    //    let currentLocation = root.Location
    //    let (moves: int * int array) = List.toArray (validMoves(currentLocation))
    //    let pick = rand.Next(Array.length moves)
    //    let nextMove = moves.[pick]
    //    nextMove

        

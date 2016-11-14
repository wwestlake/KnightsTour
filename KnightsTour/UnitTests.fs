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

module UnitTests

open System
open NUnit.Framework
open Tour

let rand = new Random((int)DateTime.Now.Ticks)

let randpos size =
    (rand.Next size, rand.Next size)    

let exists (list: (int * int) list) (p: int * int) =
    List.exists (fun e -> e = p) list


[<Test>]
let ``Validate that isPowerOf2 returns true on or false properly`` () =
    let goodTests = [ 2;4;8;16;32;64;128;256;512;1024  ]
    let badTests = [ 2;4;8;16;32;64;128;256;512;1024  ] |> List.map (fun x -> x + 1)
    do goodTests |> List.map (fun x -> Assert.IsTrue( isPowerOf2 x )) |> ignore
    do badTests |> List.map (fun x -> Assert.IsTrue( not (isPowerOf2 x) )) |> ignore


[<Test>]
let ``Validate that Board only accepts powers of 2 for size`` () =
    let b = new Board(8)
    Assert.NotNull(b) 
    try
        let a = new Board(9)
        do Assert.Fail |> ignore
    with
        | ex -> do Assert.Pass |> ignore


  
[<Test>]
 let ``Board produces valid moves that are on the board`` () =
    let n = 8
    let b = new Board(n)
    let testPositions = List.zip [0..(n-1)] [0..(n-1)]
    testPositions |> List.iter 
                        (fun pos -> b.validMoves( pos )  |> List.iter (fun (x,y) -> Assert.That(x >= 0 && y >= 0 && x < n && y < n) )) 

[<Test>]
let ``Board produces valid moves that are not taken`` () =
    let n = 8
    let b = new Board(n)
    let visited = [1..10] |> List.map (fun _ -> randpos n)
    let gb : int[,] = Array2D.init n n (fun i j -> 0)
    do visited |> List.iter (fun (x,y) -> gb.[x,y] <- 1 )
    do b.GameBoard <- gb
    let testPositions = List.zip [0..(n-1)] [0..(n-1)]
    testPositions |> List.iter (fun pos -> b.validMoves( pos ) |> List.iter (fun p -> Assert.That( not (exists visited p) )))



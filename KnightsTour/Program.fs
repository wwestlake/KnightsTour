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

open System
open Rop
open InputOutput
open CommandParser
open Tour

let rec repl () =
    do Console.Write("KT> ")
    let line = readline ()
    let result = execute line
    let result2 = output result
    match line with
    | Failure _    -> 1
    | Success (_,_) -> repl ()


[<EntryPoint>]
let main argv = 
    printfn "Knights Tour in F#"
    printfn "By William Westlake (C)2016, All Rights Reserved"
    printfn "Comments Suggestions?  wwestlake@lagdaemon.com"
    printfn "All commands begin with :"
    printfn ":help for help"

    //repl () |> ignore
    
    let b = new Board 8

    let c = b.validMoves (2,2)

    printfn "%A" c

    Console.ReadKey() |> ignore
    0 // return an integer exit code

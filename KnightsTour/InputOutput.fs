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

module InputOutput

open System
open Rop

let readline () =
    let result = Console.ReadLine();
    match result with
    | null -> fail "Reached of of file"
    | _ -> Success (result,[""])

let output result = 
    match result with
    | Success (v, _) -> printfn "%s" v
                        result
    | Failure msg    -> msg |> List.iter (printfn "%s")
                        result


// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Rop
open InputOutput
open CommandParser

let rec repl () =
    do Console.Write("KT> ")
    let line = readline
    let result = execute line
    match output result with
    | Failure msg -> msg |> List.iter printfn "%s"
    | Success (_,_) -> repl


[<EntryPoint>]
let main argv = 
    printfn "Knights Tour in F#"
    printfn "By William Westlake (C)2016, All Rights Reserved"
    printfn "Comments Suggestions?  wwestlake@lagdaemon.com"
    printfn "All commands begin with :"
    printfn ":help for help"

    repl ()
    

    Console.ReadKey()
    0 // return an integer exit code

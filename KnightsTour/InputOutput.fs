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


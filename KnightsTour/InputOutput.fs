module InputOutput

open System
open Rop

let readline () =
    let result = Console.ReadLine();
    match result with
    | null -> fail "Reached of of file"
    | _ -> succeed result

let output result = function
    | Success (v, _) -> printfn v
                        result
    | Failure msg    -> msg |> List.iter (printfn "%s")
                        result


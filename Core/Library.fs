﻿namespace Core

type Instruction = 
    | End
    | Add of (int * int * int * int)
    | Mult of (int * int * int * int)

exception GrandMa of string

module Computer =
    let translate (instructions: string) =
        instructions.Split [|','|]
        |> Seq.map(int)
    
    let translateBack (instructions: seq<int>) =
        instructions |> Seq.map(string) |> String.concat ","

    let readInstruction (instructions: int []) (pos: int) =
        match instructions.[pos] with
            | 99 -> End
            | 1 -> Add(instructions.[instructions.[pos + 1]], instructions.[instructions.[pos + 2]], instructions.[pos + 3], 4)
            | 2 -> Mult(instructions.[instructions.[pos + 1]], instructions.[instructions.[pos + 2]], instructions.[pos + 3], 4)
            | _ -> raise (GrandMa("time to fix that"))

    let update instructions pos value =
        Array.set instructions pos value
        instructions

    let rec loop (instructions: int []) (pos: int) =
        match readInstruction instructions pos with
        | End -> instructions
        | Add (x, y, where, move) -> loop (update instructions where (x+y)) (pos+move)
        | Mult (x, y, where, move) -> loop (update instructions where (x*y)) (pos+move)
    
    let realCompute (instructions: seq<int>) =
        let array =  instructions |> Seq.toArray
        (loop array 0) |> Array.toSeq

    let compute (instructions: string) =
        instructions |> translate |> realCompute |> translateBack

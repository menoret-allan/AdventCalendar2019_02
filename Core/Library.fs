﻿namespace Core

type Instruction = 
    | End
    | Overide of (int * int)

module Computer =
    let translate (instructions: string) =
        instructions.Split [|','|]
        |> Seq.map(int)
    
    let translateBack (instructions: seq<int>) =
        instructions |> Seq.map(string) |> String.concat ","

    let getValue (instructions: int []) pos =
        instructions.[instructions.[pos]]

    let readInstruction (instructions: int []) (pos: int) =
        match instructions.[pos] with
            | 99 -> Some End
            | 1 -> Some (Overide(getValue instructions (pos + 1) + getValue instructions (pos + 2), instructions.[pos + 3]))
            | 2 -> Some (Overide(getValue instructions (pos + 1) * getValue instructions (pos + 2), instructions.[pos + 3]))
            | _ -> None

    let update instructions pos value =
        Array.set instructions pos value
        instructions

    let interprete (instructions: int []) =
        let rec loop (instructions: int []) (pos: int) =
            match readInstruction instructions pos with
            | Some End -> instructions
            | Some (Overide (newValue, position)) -> loop (update instructions position newValue) (pos+4)
            | None -> [|-1|]
        loop instructions 0
    
    let realCompute (instructions: seq<int>) =
        instructions |> Seq.toArray |> interprete |> Array.toSeq

    let compute (instructions: string) =
        instructions |> translate |> realCompute |> translateBack

    let generateInstruction instructions (noun, verb) =
        let result=Array.copy instructions
        Array.set result 1 noun
        Array.set result 2 verb
        result

    let findSpecificResult gen (pair: (int * int)) =
           match interprete (gen pair) |> Array.item 0 with
           | 19690720 -> Some pair
           | _ -> None

    let findSet (instructions: string) =
        let genInstruction = instructions |> translate |> Seq.toArray |> generateInstruction |> findSpecificResult
        let pairs = seq {for noun in 0..100 do yield! seq {for verb in 0..100 do (noun , verb) } }
        pairs |> Seq.choose(genInstruction) |> Seq.exactlyOne 


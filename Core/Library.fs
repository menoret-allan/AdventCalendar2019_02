namespace Core

type Instruction = 
    | Error
    | End
    | Add of (int * int)
    | Mult of (int * int)

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
            | 1 -> Add(instructions.[instructions.[pos + 1]] + instructions.[instructions.[pos + 2]], instructions.[pos + 3])
            | 2 -> Mult(instructions.[instructions.[pos + 1]] * instructions.[instructions.[pos + 2]], instructions.[pos + 3])
            | _ -> Error

    let update instructions pos value =
        Array.set instructions pos value
        instructions

    let rec loop (instructions: int []) (pos: int) =
        match readInstruction instructions pos with
        | End -> instructions
        | Add (newValue, position)
        | Mult (newValue, position) -> loop (update instructions position newValue) (pos+4)
        | Error -> [|-1|]
    
    let realCompute (instructions: seq<int>) =
        let array =  instructions |> Seq.toArray
        (loop array 0) |> Array.toSeq

    let compute (instructions: string) =
        instructions |> translate |> realCompute |> translateBack

    let generateInstruction instructions (noun, verb) =
        let result=Array.copy instructions
        Array.set result 1 noun
        Array.set result 2 verb
        result


    let findSet (instructions: string) =
        let values = instructions |> translate |> Seq.toArray
        let pairs = seq {for noun in 0..100 do yield! seq {for verb in 0..100 do (noun , verb) } }
        let rec testing (pair: (int * int)) =
               match loop (generateInstruction values pair) 0 |> Array.item 0 with
               | 19690720 -> (true, pair)
               | _ -> (false, pair)
        let (_, pair) = pairs |> Seq.map(testing) |> Seq.filter(fun (is, pair) -> is) |> Seq.exactlyOne 
        pair













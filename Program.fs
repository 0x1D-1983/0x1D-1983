open System.Threading.Tasks
open System


type Operation = Add | Sub | Div | Mul | Pow

type Calculator = 
    | Value of double
    | Expression of Operation * Calculator * Calculator

let spawn (op: unit -> double) = Task.Run(op)


let rec eval exp = 
     match exp with
        | Value value -> value
        | Expression (op, lExp, rExp) ->

            let op1 = spawn (fun () -> eval lExp)
            let op2 = spawn (fun () -> eval rExp)

            let apply = Task.WhenAll([op1; op2])

            let lRes, rRes = apply.Result.[0], apply.Result.[1]

            match op with
            | Add -> lRes + rRes
            | Sub -> lRes - rRes
            | Div -> lRes / rRes
            | Mul -> lRes * rRes
            | Pow -> Math.Pow(lRes, rRes)

let calculation =
    Expression (Add,
        Expression (Div,
            Expression (Pow, Value (2.0), Value (10.0)),
            Expression (Pow, Value (2.0), Value (9.0))),
        Expression (Mul, Value (2.0), Value (2.0)))



[<EntryPoint>]
eval calculation |> printfn "%A"
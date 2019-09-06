#light
[<AutoOpen>]
module DFA

open System
open System.Diagnostics
open System.Collections.Generic

type DFAResult = Continue = 0
               | Quit = 1
               | ElseQuit = 2
               | End = 3

[<AbstractClass>]
type DFAState<'Token, 'Function>() =
    member x.NoFunction = true
    member x.m_Id = 0
    member val Func = Unchecked.defaultof<'Function> with get, set
    member x.IsQuitState = false
    member x.ElseStateId = -1
    member val NextStateIds : int array option = None with get, set
    member x.NextStateIdDict : IDictionary<int, int> option = None

    member x.AddNextState action nextstate =
        Debug.Assert(action >= 0)
        match x.NextStateIdDict with
        | Some dict -> dict.Add(action, nextstate)
        | None ->
            match x.NextStateIds with
            | None ->
                x.NextStateIds <- Some(Array.create (action + 1) -1)
            | Some arr ->
                match arr.Length < action + 1 with
                | true ->
                    let oldArr = x.NextStateIds.Value
                    let newArr = Array.create (action + 1) -1
                    Array.Copy(oldArr, newArr, oldArr.Length)
                    x.NextStateIds <- Some newArr
                | _ -> ()
            x.NextStateIds.Value.[action] <- nextstate

//    member x.NextState(action)
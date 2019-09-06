#light

[<AutoOpen>]
module Collections

open System.Collections
open System.Collections.Generic

type LazyList<'T> =  Empty
                   | Cons of 'T * (unit -> LazyList<'T>)
                   | Delay of (unit -> LazyList<'T>)
                   | Combine of LazyList<'T> * LazyList<'T> with
    interface IEnumerable<'T> with
        member self.GetEnumerator() =
            // tail-recursive enumeration
            let rec toSeq stack =
                match stack with
                | [] -> Seq.empty
                | head :: tail ->
                    match head with
                    | Empty -> toSeq tail
                    | Cons (value, rest) -> seq {yield value; yield! toSeq <| rest() :: tail }
                    | Delay f -> toSeq <| f () :: tail
                    | Combine (first, second) -> toSeq <| first :: second :: tail
            (toSeq [self]).GetEnumerator()
    interface IEnumerable with
        member self.GetEnumerator() = (self :> IEnumerable<'T>).GetEnumerator() :> _

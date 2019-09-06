#light

[<AutoOpen>]
module GenericFunctor
open System
//
/// Generic container of 'T
/// Also parameterized by 'TypeClass : (new: unit -> 'TypeClass)  e.g. let tc = new 'TypeClass()
/// Note that 'TypeClass is a builder of workflow in which the base computation type(unwrapped type) is 'T
/// and Generic<'T, ...> is a wrapper type of 'T
type Generic<'T, 'TypeClass when 'TypeClass: (new: unit -> 'TypeClass)> = interface end

type [<AbstractClass>] FunctorClass<'FunctorClass when 'FunctorClass :> FunctorClass<'FunctorClass> and
                                                   'FunctorClass :  (new : unit -> 'FunctorClass)>() =
    abstract FMap<'T, 'R> : ('T -> 'R) -> Generic<'T, 'FunctorClass> -> Generic<'R, 'FunctorClass>

type [<AbstractClass>] AppClass<'AppClass when 'AppClass :> AppClass<'AppClass> and
                                          'AppClass :  (new : unit -> 'AppClass)>() =
    inherit FunctorClass<'AppClass>()
    abstract Pure<'T> : 'T -> Generic<'T, 'AppClass>
    abstract Apply<'T, 'R> : Generic<'T -> 'R, 'AppClass> -> Generic<'T, 'AppClass> -> Generic<'R, 'AppClass>
    override this.FMap<'T, 'R> (f : 'T -> 'R) (fa: Generic<'T, 'AppClass>) : Generic<'R, 'AppClass> =
        this.Apply (this.Pure f) fa

type [<AbstractClass>] MonadClass<'MonadClass when 'MonadClass :> MonadClass<'MonadClass> and
                                                'MonadClass :  (new : unit -> 'MonadClass)>() =
    inherit AppClass<'MonadClass>()
    abstract Return<'T> : 'T -> Generic<'T, 'MonadClass>
    abstract Bind<'T, 'R> : Generic<'T, 'MonadClass> * ('T -> Generic<'R, 'MonadClass>) -> Generic<'R, 'MonadClass>
    member this.ReturnFrom<'T> (t: 'T) = t

    member this.Then<'T, 'R> ((ma: Generic<'T, 'MonadClass>), (mb: Generic<'R, 'MonadClass>)) : Generic<'R, 'MonadClass> =
        this.Bind(ma, fun _ -> mb)
    override this.Pure<'T> (value : 'T) : Generic<'T, 'MonadClass> = this.Return value
    override this.Apply<'T, 'R> (mf: Generic<'T -> 'R, 'MonadClass>) (ma: Generic<'T, 'MonadClass>) : Generic<'R, 'MonadClass> =
        this.Bind(mf, fun f -> this.Bind(ma, fun a -> this.Pure (f a)))

type Maybe<'T> = None | Some of 'T with
    interface Generic<'T, MaybeClass>
and MaybeClass() =
    inherit MonadClass<MaybeClass>() with
        override this.Return<'T>(t: 'T) = Some t :> _
        override this.Bind<'T, 'R> ((m : Generic<'T, MaybeClass>), (f: ('T -> Generic<'R, MaybeClass>))) : Generic<'R, MaybeClass> =
            match m :?> _ with
            | Some v -> f v
            | None -> None :> _

type ListMonad<'T> = ListMonad of LazyList<'T> with
    interface Generic<'T, ListMonadClass>
    member this.ToList() =
        match this with
        | ListMonad lazyList -> lazyList |> LazyList.toList

and ListMonadClass() =
    inherit MonadClass<ListMonadClass>() with
        override this.Return<'T> (t: 'T) = ListMonad (LazyList.ofList [t]) :> _
        override this.Bind<'T, 'R> ((l: Generic<'T, ListMonadClass>), (f: ('T -> Generic<'R, ListMonadClass>))) : Generic<'R, ListMonadClass> =
            let (ListMonad list) = l :?> _ in
                ListMonad (LazyList.ofSeq <| Seq.collect (fun v -> let (ListMonad list') = (f v) :?> _ in list') list) :> _


type ListApp<'T> = ListApp of LazyList<'T> with
    interface Generic<'T, ListAppClass>
    member this.ToList() =
        match this with
        | ListApp lazyList -> lazyList |> LazyList.toList
and ListAppClass() =
    inherit AppClass<ListAppClass>() with
        override this.Pure<'T> (t : 'T) = ListApp (LazyList.repeat t) :> _
        override this.Apply<'T, 'R> (ff: Generic<'T -> 'R, ListAppClass>) (fa : Generic<'T, ListAppClass>) : Generic<'R, ListAppClass> =
            let (ListApp listf) = ff :?> _ in
            let (ListApp list) = fa :?> _ in
                ListApp (LazyList.map (fun (f, a) -> f a) (LazyList.zip listf list)) :> _

let maybe = new MaybeClass() :> MonadClass<MaybeClass>

let ``pure``<'T, 'AppClass when 'AppClass :> AppClass<'AppClass>
                       and  'AppClass :  (new : unit -> 'AppClass)>
    (t : 'T) : Generic<'T, 'AppClass> = (new 'AppClass()).Pure t

let apply<'T, 'R, 'AppClass when 'AppClass :> AppClass<'AppClass>
                            and  'AppClass :  (new : unit -> 'AppClass)>
    (ff : Generic<'T -> 'R, 'AppClass>) (fa : Generic<'T, 'AppClass>) : Generic<'R, 'AppClass> =
    (new 'AppClass()).Apply ff fa

let (<*>) ff fa = apply ff fa

let ($) f fa = ``pure`` f <*> fa

let (<.>) fa fb = (fun a b -> a, b) $ fa <*> fb

// ziplist example
let rec transpose (listoflist : LazyList<LazyList<'T>>) : Generic<LazyList<'T>, ListAppClass> =
    match listoflist with
    | LazyList.Nil -> ``pure`` LazyList.empty
    | LazyList.Cons (xs, xss) -> LazyList.cons $ (ListApp xs) <*> transpose xss

let unit<'T, 'MonadClass when 'MonadClass :> MonadClass<'MonadClass>
                         and  'MonadClass :  (new : unit -> 'MonadClass)>
    (t : 'T) : Generic<'T, 'MonadClass> = (new 'MonadClass()).Return t

let bind<'T, 'R, 'MonadClass when 'MonadClass :> MonadClass<'MonadClass>
                             and  'MonadClass :  (new : unit -> 'MonadClass)>
    (m: Generic<'T, 'MonadClass>) (f: 'T -> Generic<'R, 'MonadClass>) : Generic<'R, 'MonadClass> =
    (new 'MonadClass()).Bind(m, f)

let (>>=) = bind
let (>>) ma mb = ma >>= fun _ -> mb

let rec sequence (list : Generic<'T, 'MonadClass> list) : Generic<'T list, 'MonadClass> =
    match list with
    | [] -> unit []
    | h :: tail -> h >>= fun v -> sequence tail >>= fun vs -> unit (v :: vs)

let map (f : 'T -> Generic<'R, 'MonadClass>) (list : 'T list) : Generic<'R list, 'MonadClass> =
    (sequence << List.map f) list

let rec filter (p : 'T -> Generic<bool, 'MonadClass>) (list: 'T list) : Generic<'T list, 'MonadClass> =
    match list with
    | [] -> unit []
    | x :: xs -> p x >>= fun b -> filter p xs >>= fun ys -> if b then unit (x :: ys) else unit ys


#light
namespace Monad

type IO<'T> = private | Action of (unit -> 'T)

module IOMonad =
    let private raw (Action f) = f
    let private run io         = raw io ()
    let private eff g io       = raw io () |> g
    let private bind io rest   = Action(fun () -> io |> eff rest |> run)
    let private comb io1 io2   = Action(fun () -> run io1; run io2)

    type IOBuilder() =
        member b.Return(x)              = Action(fun () -> x)
        member b.ReturnFrom(io) : IO<_> = io
        member b.Delay(g) : IO<_>       = g ()
        member b.Bind(io, rest)         = bind io rest
        member b.Combine(io1, io2)      = comb io1 io2

    let io = new IOBuilder()
    let (|Action|) io = run io

module PreludeIO =
    let putChar (c: char)    = Action (fun () -> stdout.Write c)
    let putStr (s: string)   = Action (fun () -> stdout.Write s)
    let putStrLn (s: string) = Action (fun () -> stdout.WriteLine s)
    let print x              = Action (fun () -> printfn "%A" x)
    let getChar              = Action (fun () -> stdin.Read() |> char |> string)
    let getLine              = Action (fun () -> stdin.ReadLine())
    let getContents          = Action (fun () -> stdin.ReadToEnd())
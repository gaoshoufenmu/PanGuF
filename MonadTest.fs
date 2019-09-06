module MonadTest

open Xunit

type ``All Monad``() =
    
    [<Fact>]
    member this.``1+1``() =
        Assert.Equal(Some(2) :> Generic<int, MaybeClass>, maybe { return 1 } >>= fun x -> maybe { return x + 1 })

    [<Fact>]
    member this.``1+"2"``() =
        Assert.Equal(Some("12") :> Generic<string, MaybeClass>, maybe{ return 1 } >>= fun x -> maybe {return x.ToString() + "2"})

    [<Fact>]
    member this.``[wrap]->wrap([])``() =
        Assert.Equal(Some [1; 2; 3] :> Generic<int list, MaybeClass>, sequence [maybe { return 1 }; maybe { return 2 }; maybe { return 3 } ])

    [<Fact>]
    member this.``map T->Generic Tlist``() =
        Assert.Equal(Some [2 .. 2 .. 10] :> Generic<int list, MaybeClass>, map (fun v -> maybe { return v * 2 }) [1 .. 5])

    [<Fact>]
    member this.``filter T->Generic TList``() =
        Assert.Equal(Some [2; 4] :> Generic<int list, MaybeClass>, filter (fun v -> maybe { return  v &&& 0x00000001 = 0 }) [1 .. 5])

    [<Fact>]
    member this.``zip listoflist``() =
        let matrix = [[1;4];[2;5];[3;6]] |> List.map LazyList.ofList |> LazyList.ofList |> ListApp
        let v = matrix.ToList() |> List.map (fun x -> x |> LazyList.toList) |> List.concat
        let matrix' = [[1;2;3];[4;5;6]] |> LazyList.ofList |> LazyList.map LazyList.ofList |> transpose :?> ListApp<LazyList<int>>
        let v' = matrix'.ToList() |> List.map (fun x -> x |> LazyList.toList) |> List.concat
        Assert.Equal<int>(v, v')

    [<Fact>]
    member this.``list monad``() =
        let onetoten = ListMonad (LazyList.ofList [1 .. 3])
        let monad = (fun a b -> sprintf "%d * %d = %d" a b (a * b)) $ onetoten <*> onetoten :?> ListMonad<string>
        let list = monad.ToList()
        let list' = [ for i in [1 .. 3] do for j in [1 ..3] do yield (sprintf "%d * %d = %d" i j (i * j)) ]
        Assert.Equal<string>(list, list')
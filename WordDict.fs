module WordDict

open System
open System.IO

type WordDictionary private () =
    let mutable _trie = Map.empty<string, double>
    let mutable _total = 0.0;

    static let addPair m (x : string, y : double) = 
        match Map.tryFind x m with
        | Some (l) -> m
        | None -> Map.add x y m

    static let lineParse (line : string) = 
        match line.Split(' ') with
        | [|x; y|] -> Some(x, float y)
        | _ -> None
    
    static let subSetStrs (input: string) = 
        [1..input.Length - 1] |> List.map(fun x -> input.Substring(0, x))
    
    static let updateDict (tuple_op : (string*double)option) map total =
        match tuple_op with
        | Some (word, freq) -> let nmap = subSetStrs word |> List.fold (fun m x -> addPair m (x, 0.0)) (Map.add word freq map)
                               nmap, total + freq
        | _ -> map, total

    static let loadDict ()= 
        try
            File.ReadLines(Config.maindict_path()) |> Seq.map (fun line -> lineParse line)
                |> Seq.fold (fun (map, total) tuple_op -> updateDict tuple_op map total) (Map.empty<string, double>, 0.0)
        with
        | ex -> raise ex

    static let createSingleton () = 
        let dict = new WordDictionary()
        let trie, total = loadDict()
        dict.Trie <- trie
        dict.Total <- total
        dict

    static let instance = lazy (createSingleton())
    static member Instance = instance.Value
    member x.Total with get() = _total and set total = _total <- total
    member x.Trie with get() = _trie and set trie = _trie <- trie
    

    member x.containsWord word = 
        match x.Trie.TryFind word with
        | Some freq -> if freq > 0.0 then true else false
        | _ -> false

    member x.GetFreq word =
        match x.Trie.TryFind word with
        | Some freq -> if freq > 0.0 then freq else 1.0
        | _ -> 1.0

    member x.addWord word freq =
        match x.containsWord word with
        | true -> _total <- _total - x.Trie.[word]
        | _ -> ()

        _trie <- subSetStrs word |> List.fold (fun m x -> addPair m (x, 0.0)) _trie
        _total <- _total + freq

    member x.deleteWord word = x.addWord word 0.0

    member x.SuggestFreq word (segs:string list) = 
        segs |> List.fold (fun f s -> x.GetFreq(s) / _total * f) 1.0 |> (+) 1.0 |> max (x.GetFreq(word))

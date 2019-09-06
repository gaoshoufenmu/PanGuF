module EntSegment

open System
open System.Text.RegularExpressions
open Utils

let re_chs = Regex @"([\u4E00-\u9FD5]+)"
let word_dict = WordDict.WordDictionary.Instance

let cut_it (blk: string) (cut_func: string -> string list) =
        if String.IsNullOrWhiteSpace(blk) then []
        else
            match re_chs.IsMatch blk with
            | true -> cut_func blk
            | _ -> [blk]

let get_dag (text: string) =
    let rec get_substr_dag k i N (sentence: string) (temp_list: int list) =
        match i < N with
        | false -> if temp_list.Length > 0 then temp_list else [k]
        | _ ->
            let frag = text.Substring(k, i+1 - k)
            let new_list = if word_dict.Trie.ContainsKey frag && word_dict.Trie.[frag] > 0.0 then temp_list @ [i] else temp_list
            get_substr_dag k (i+1) N sentence new_list
    text.ToCharArray() |> Array.mapi (fun k c -> (k, get_substr_dag k k text.Length text [])) |> List.ofArray

let calc (sentence: string) (dag : (int * int list) list) =
    let logtotal = Math.Log word_dict.Total
    Map.empty<int, int * double>.Add(sentence.Length, (0, 0.0)) |> List.foldBack
        (fun (i, nexts) (route: Map<int, int *double>) -> 
                nexts |> List.map (fun x -> 
                    x, sentence.Substring(i, x + 1 - i) |> word_dict.GetFreq |> Math.Log |> (+) ((route.[x+1] |> snd) - logtotal)
                ) |> List.maxBy (fun (k, v) -> v) |> composeTuple i |> route.Add
        )
        dag

// In reversed order to add each segment into words
let addBuffer2WordList (words: string list) (buf: string) =
    match buf.Length with
    | 1 ->  buf :: words
    |_ -> 
        if word_dict.containsWord buf
        then buf |> Seq.fold (fun ws s -> s.ToString() :: ws) words
        else
            FinalSeg.cut buf |> List.fold (fun ws s -> s :: ws) words
            
let cut_dag (text: string) =
    let n = text.Length
    let dag = get_dag text
    let route = calc text dag
    let rec addWords x words (buf: string) =
        match x < n with
        | false -> 
            match buf.Length > 0 with
            | false -> words
            | _ -> addBuffer2WordList words buf
        | _ ->
            let y = fst route.[x] + 1
            let w = text.Substring(x, y - x)
            match y - x with
            | 1 -> addWords y words (buf + w)
            | _ ->
                let tempWords = if buf.Length > 0 then addBuffer2WordList words buf else words
                addWords y (w :: tempWords) String.Empty

    addWords 0 [] String.Empty |> List.rev  // at the last step, string list should be reversed

let cut text = 
    re_chs.Split text |>  Array.fold (fun res blk -> res @ cut_it blk cut_dag) []

let cut4csharp text = cut text |> Array.ofList


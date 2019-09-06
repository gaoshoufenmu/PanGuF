module Config

let mutable _isref_byfsharp = true
let set_caller_mode isref_byfsharp = _isref_byfsharp = isref_byfsharp |> ignore

let maindict_path()= 
    if _isref_byfsharp then __SOURCE_DIRECTORY__ + @"\Resources\dict.txt" else @".\Resources\dict.txt"

let ProbTransFile = maindict_path() + "prob_trans.json"
let ProbEmitFile = maindict_path() + "prob_emit.json"
let PosProbStartFile = maindict_path() + "pos_prob_start.json"
let PosProbTransFile = maindict_path() + "pos_prob_trans.json"
let PosProbEmitFile = maindict_path() + "pos_prob_emit.json"
let CharStateTabFile = maindict_path() + "char_state_tab.json"
let StopwordsFile = maindict_path() + "stopwords.txt"
let IdfFile = maindict_path() + "idf.txt"
let ArticleFile = maindict_path() + "article.txt"


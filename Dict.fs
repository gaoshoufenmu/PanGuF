#light
[<AutoOpen>]
module Dict

open System
open System.Text
open System.IO
open System.Reflection
open System.Collections.Generic


let SingleNameFileName = "ChsSingleName.txt"
let DoubleNameAFileName = "ChsDoubleNameA.txt"
let DoubleNameBFileName = "ChsDoubleNameB.txt"

let arr = '\n' :: []

/// 姓氏
let FAMILY_NAMES = [|
        //有明显歧异的姓氏
        "王";"张";"黄";"周";"徐";
        "胡";"高";"林";"马";"于";
        "程";"傅";"曾";"叶";"余";
        "夏";"钟";"田";"任";"方";
        "石";"熊";"白";"毛";"江";
        "史";"候";"龙";"万";"段";
        "雷";"钱";"汤";"易";"常";
        "武";"赖";"文"; "查";

        //没有明显歧异的姓氏
        "赵"; "肖"; "孙"; "李";
        "吴"; "郑"; "冯"; "陈"; 
        "褚"; "卫"; "蒋"; "沈"; 
        "韩"; "杨"; "朱"; "秦"; 
        "尤"; "许"; "何"; "吕"; 
        "施"; "桓"; "孔"; "曹";
        "严"; "华"; "金"; "魏";
        "陶"; "姜"; "戚"; "谢";
        "邹"; "喻"; "柏"; "窦";
        "苏"; "潘"; "葛"; "奚";
        "范"; "彭"; "鲁"; "韦";
        "昌"; "俞"; "袁"; "酆"; 
        "鲍"; "唐"; "费"; "廉";
        "岑"; "薛"; "贺"; "倪";
        "滕"; "殷"; "罗"; "毕";
        "郝"; "邬"; "卞"; "康";
        "卜"; "顾"; "孟"; "穆";
        "萧"; "尹"; "姚"; "邵";
        "湛"; "汪"; "祁"; "禹";
        "狄"; "贝"; "臧"; "伏";
        "戴"; "宋"; "茅"; "庞";
        "纪"; "舒"; "屈"; "祝";
        "董"; "梁"; "杜"; "阮";
        "闵"; "贾"; "娄"; "颜";
        "郭"; "邱"; "骆"; "蔡";
        "樊"; "凌"; "霍"; "虞";
        "柯"; "昝"; "卢"; "柯";
        "缪"; "宗"; "丁"; "贲";
        "邓"; "郁"; "杭"; "洪";
        "崔"; "龚"; "嵇"; "邢";
        "滑"; "裴"; "陆"; "荣";
        "荀"; "惠"; "甄"; "芮";
        "羿"; "储"; "靳"; "汲"; 
        "邴"; "糜"; "隗"; "侯";
        "宓"; "蓬"; "郗"; "仲";
        "栾"; "钭"; "历"; "戎";
        "刘"; "詹"; "幸"; "韶";
        "郜"; "黎"; "蓟"; "溥";
        "蒲"; "邰"; "鄂"; "咸";
        "卓"; "蔺"; "屠"; "乔";
        "郁"; "胥"; "苍"; "莘";
        "翟"; "谭"; "贡"; "劳";
        "冉"; "郦"; "雍"; "璩";
        "桑"; "桂"; "濮"; "扈";
        "冀"; "浦"; "庄"; "晏";
        "瞿"; "阎"; "慕"; "茹";
        "习"; "宦"; "艾"; "容";
        "慎"; "戈"; "廖"; "庾";
        "衡"; "耿"; "弘"; "匡";
        "阙"; "殳"; "沃"; "蔚";
        "夔"; "隆"; "巩"; "聂";
        "晁"; "敖"; "融"; "訾";
        "辛"; "阚"; "毋"; "乜";
        "鞠"; "丰"; "蒯"; "荆";
        "竺"; "盍"; "单"; "欧";

        //复姓必须在单姓后面
        "司马"; "上官"; "欧阳";
        "夏侯"; "诸葛"; "闻人";
        "东方"; "赫连"; "皇甫";
        "尉迟"; "公羊"; "澹台";
        "公冶"; "宗政"; "濮阳";
        "淳于"; "单于"; "太叔";
        "申屠"; "公孙"; "仲孙";
        "轩辕"; "令狐"; "徐离";
        "宇文"; "长孙"; "慕容";
        "司徒"; "司空"; "万俟" |]

let GetFamilyNameMap (names: string[]) = 
        let mutable _familyNameMap = new Dictionary<char, char list option>()
        for name in names do
            match name.Length with
            | 1 -> if _familyNameMap.ContainsKey name.[0] then _familyNameMap.Add(name.[0], None) else ()
            | _ -> match _familyNameMap.ContainsKey name.[0] with
                   | true -> match _familyNameMap.[name.[0]] with
                             | None -> _familyNameMap.[name.[0]] <- Some((char)0 :: [])
                             | Some(x) -> _familyNameMap.[name.[0]] <- Some(name.[1] :: x)
                   | false -> _familyNameMap.[name.[0]] <- Some(name.[1] :: [])
        _familyNameMap

let FamilyNameMap = GetFamilyNameMap FAMILY_NAMES

let DictGetValue (dict: Dictionary<char, char list option>) key =
    let mutable v = Unchecked.defaultof<char list option>
    if(dict.TryGetValue(key, &v))
    then (true, v)
    else
        (false ,None)

let DictGetVal (dict: Dictionary<'k, 'v>) key =
    let mutable v = Unchecked.defaultof<'v>
    if(dict.TryGetValue(key, &v))
    then true, Some v
    else false, None

/// Chinese Name
type ChsName() =

    let LoadNameDict filePath =
        let dict = new Dictionary<char, char>()
        let items = (ReadFileToString filePath Encoding.UTF8).Value |> Split @"\r\n"
        for item in items do
            if(String.IsNullOrEmpty(item)) 
            then ()
            else
                if(dict.ContainsKey(item.[0])) then () else dict.Add(item.[0], item.[0])
        dict
    
    let rec MatchName key (list:char list) =
        match list with
        | [] -> 0                            // not found
        | c :: tail ->
            match c with
            | x when x = key -> 2           // double name
            | y when y = (char)0 -> 1       // single name
            | _ -> MatchName key tail
        

//    let MatchR (text: string) start =
        

//    member val FamilyNameMap = null with get, set
    member val SingleNameMap: Dictionary<char, char> = null with get, set
    member val DoubleNameAMap = null with get, set
    member val DoubleNameBMap = null with get, set

    member this.LoadChsName dictPath =
        let path = AppendDivision dictPath '\\'
        this.SingleNameMap <- LoadNameDict (dictPath + SingleNameFileName)
        this.DoubleNameAMap <- LoadNameDict (dictPath + DoubleNameAFileName)
        this.DoubleNameBMap <- LoadNameDict (dictPath + DoubleNameBFileName)

    member this.Match (text: string) start =
        // Find name from text
        let InternalMatch key (text : string) offset =
            match start + offset >= text.Length with
            | true -> None
            | _ ->
                seq { if this.SingleNameMap.ContainsKey key then yield text.Substring(start, offset + 1)
                      if this.DoubleNameAMap.ContainsKey key && start + offset + 1 < text.Length then
                          let cur = text.[start + offset + 1]
                          if this.DoubleNameBMap.ContainsKey cur then yield text.Substring(start, offset + 2)
                } |> List.ofSeq |> Some

        match start > text.Length - 2 with
        | true -> None
        | _ ->
            // Find family name from text, and then find name from text
            match DictGetValue FamilyNameMap text.[start] with
            | (false, _) -> None
            | (true, op) ->
                match op with
                | Some list ->
                    match MatchName text.[start + 1] list with
                    | 0 -> None
                    | x -> InternalMatch text.[start + x] text x
                | _ -> InternalMatch text.[start + 1] text 1



type StopWord() =
    let mutable _stopwordMap = Map.empty<string, string>
    member this.StopwordMap
        with get() = _stopwordMap
        and set y = _stopwordMap <- y

    /// <summary>judge if a word is a stopword</summary>
    /// <param name="filterEng">a boolean to jugde if filter a english char</param>
    member this.IsStopword word filterEng filterEngLen filterNumeric filterNumLen =
        match String.IsNullOrEmpty word with
        | true -> false
        | _ ->
            match word.[0] < (char)128 with     // word is ascii
            | true ->
                let key = word.ToLower()
                
                match filterEng && (word.Length > filterEngLen && (word.[0] < '0' || word.[0] > '9')) with
                | true -> true  // if english word(nonnumeric) whose length larger than given 'filterEngLen', then it is a stopword
                | _ ->          // else if word is numeric whose length larger than given 'filterNumLen', then it is a stopword
                    if filterNumeric && (word.Length > filterNumLen && (word.[0] >= '0' && word.[0] <= '9'))
                    then true
                    else _stopwordMap.ContainsKey key
            | _ -> _stopwordMap.ContainsKey word
                
    member this.LoadStopwordMap fileName =
        let stream = option<Stream>.None
        let sr =
            match File.Exists fileName with
            | true -> new StreamReader(fileName, Encoding.UTF8)
            | _ ->
                let assembly = Assembly.GetExecutingAssembly()
                let assemblyName = assembly.GetName().Name
                let stream = assembly.GetManifestResourceStream(assemblyName + ".Resources." + Path.GetFileName(fileName))
                                |> Some
                new StreamReader(stream.Value, Encoding.UTF8)

        // Load chinese stopword
        let ``seq`` = 
            seq { while not sr.EndOfStream do
                      let stopword = sr.ReadLine()      // read the file one line by one line
                      if(not (String.IsNullOrEmpty(stopword)))
                      then
                          let key = if(stopword.[0] < (char)128) then stopword.ToLower() else stopword
                          yield (key, stopword)
            } |> Seq.distinct
        sr.Close()
        if (stream.IsSome) then stream.Value.Close() else ()
        _stopwordMap <- ``seq`` |> Map.ofSeq

type Synonym() =
    let _lock = new Object()

    let SynonymFileName = "Synonym.txt"

    let groups = new Dictionary<string, List<int>>()
    let mutable arrayList = []

    let mutable init = false

    let LoadSynonym fileName =
        match File.Exists fileName with
        | false -> ()
        | _ ->
            arrayList <-
                using (new StreamReader(fileName, Encoding.UTF8)) (fun x ->
                    [ while(not (x.EndOfStream)) do
                        let line = x.ReadLine().Trim().ToLower()
                        if(not (String.IsNullOrEmpty line))
                        then
                            yield line.Split(Array.create 1 ',')
                    ])

            let rec Group index (list: string [] list) =
                match list with
                | [] -> ()
                | array :: tail ->
                    for a in array do
                        let w = a.Trim()
                        match DictGetVal groups w with
                        | true, Some dict ->
                            if(dict.[dict.Count - 1] = index) then () else dict.Add index
                        | _ ->
                            let l = new List<int>(1)
                            l.Add index
                            groups.Add(w, l)
                    Group (index + 1) tail
            Group 0 arrayList
            init <- true


    member val ArrayList = arrayList with get, set
    member val Groups = groups with get, set
    member this.Init with get() = lock _lock (fun() -> init)
    member this.Load dictPath = LoadSynonym dictPath
    member private this.Load_ () =
        lock _lock (fun() -> if(not this.Init)
                                then
                                    ()
                                else ())


type SearchWordResult() =
    // 单词
    member val Word : WordAttribute option = Option.None with get, set
    // 相似度
    member val SimilarRatio = 0.0f with get, set
    override x.ToString() = match x.Word with | Some word -> word.Word | _ -> ""

    interface IComparable with
        override x.CompareTo obj =
            let other = downcast obj : SearchWordResult
            if x.SimilarRatio = other.SimilarRatio
            then 0
            else
                if x.SimilarRatio > other.SimilarRatio
                then  -1
                else 1

[<Serializable>]
type WordDictionaryFile() =
    member val Dicts = List.empty<WordAttribute> with get, set

type PositionLength =
    struct
        val Level:int
        val Position: int
        val Length : int
        val WordAttr : WordAttribute

        new(position, length, wordAttr) = { Level = 0; WordAttr = wordAttr; Length = length; Position = position }
        override x.ToString() = sprintf "%s,%d" x.WordAttr.Word x.Position
    end

/// Dictionary for word
type WordDictionary() =
    let mutable _version = "00"
    let SaveWordAttr2Stream (v : WordAttribute, fs: FileStream) =
        let word = Encoding.UTF8.GetBytes(v.Word)
        let pos = BitConverter.GetBytes(int v.Pos)
        let frequency = BitConverter.GetBytes(v.Frequency)
        let length = BitConverter.GetBytes(word.Length + pos.Length + frequency.Length)
        fs.Write(length, 0, length.Length)
        fs.Write(word, 0, word.Length)
        fs.Write(pos, 0, pos.Length)
        fs.Write(frequency, 0, frequency.Length)

    member val WordDict = Map.empty<string, WordAttribute>
    member val FirstCharDict = Map.empty<char, WordAttribute>
    member val DoubleCharDict = Map.empty<uint32, WordAttribute>
    member val TripleCharDict = Map.empty<int64, byte[]>

    member private x.LoadFromTextFile (fileName: string) =
        let words =
            using (new StreamReader(fileName, Encoding.UTF8))
                (fun sr -> 
                    [ while not sr.EndOfStream do
                        let strs = sr.ReadLine().Split([| '|' |])
                        if strs.Length = 3 then
                            let word = strs.[0].Trim()
                            let pos : POS = enum (Int32.Parse(strs.[1].Substring(2, String.length(strs.[1]) - 2), System.Globalization.NumberStyles.HexNumber))
                            let frequency = Double.Parse(strs.[2])
                            yield new WordAttribute(word, pos, frequency) ])
        let dictFile = new WordDictionaryFile()
        dictFile.Dicts <- words
    
    // fileName is full path
    member private x.LoadFromBinFile (fileName : string) =
        let stream = if File.Exists fileName 
                     then new FileStream(fileName, FileMode.Open, FileAccess.Read)
                     else ReadResourceToStream(Path.GetFileName fileName) :?> FileStream
        let version = Array.zeroCreate<byte> 32
        stream.Read(version, 0, version.Length) |> ignore
        let ver = Encoding.UTF8.GetString(version, 0, version.Length)
        let zeroIndex = ver.IndexOf '\u0000'
        let verNet = if zeroIndex >= 0 then ver.Substring(0, zeroIndex) else ver
        let verNumStr = Regex.GetMatch verNet "Pan Gu Segment V(.+)" true
        let dictFile = new WordDictionaryFile()
        dictFile.Dicts <-
            [ while stream.Position < stream.Length do
                let buf = Array.zeroCreate<byte> sizeof<int>
                stream.Read(buf, 0, buf.Length) |> ignore
                let length = BitConverter.ToInt32(buf, 0)
                let bufNext = Array.zeroCreate<byte> length
                stream.Read(bufNext, 0, length) |> ignore
                let word = Encoding.UTF8.GetString(bufNext, 0, length - sizeof<int> - sizeof<double>)
                let pos : POS = enum (BitConverter.ToInt32(bufNext, length - sizeof<int> - sizeof<double>))
                let frequency = BitConverter.ToDouble(bufNext, length - sizeof<double>)
                let wordAttr = new WordAttribute(word, pos, frequency)
                String.Intern(wordAttr.Word) |> ignore
                yield wordAttr]
        stream.Close()
        verNumStr, dictFile

    member private x.SaveToTextFile (fileName : string) =
        using (new FileStream(fileName, FileMode.Create)) (fun fs -> 
            using (new StreamWriter(fs, Encoding.UTF8)) (fun sw ->
                x.FirstCharDict |> Map.iter (fun k v -> sw.WriteLine(String.Format("{0}|0x{1:x4}|{2}", v.Word, uint32 v.Pos, v.Frequency)))
                x.DoubleCharDict |> Map.iter (fun k v -> sw.WriteLine(String.Format("{0}|0x{1:x4}|{2}", v.Word, uint32 v.Pos, v.Frequency)))
                x.WordDict |> Map.iter (fun k v -> sw.WriteLine(String.Format("{0}|0x{1:x4}|{2}", v.Word,  uint32 v.Pos, v.Frequency)))))

    member private x.SaveToBinFile(fileName : string, verStr : string) =
        using (new FileStream(fileName, FileMode.Create)) (fun fs ->
            let ver = if String.length verStr > 8 then verStr.Substring(0, 8) else verStr
            let version = [| for b in Encoding.UTF8.GetBytes("Pan Gu Segment V" + ver) do yield b |]
            fs.Write(version, 0, version.Length)

            x.FirstCharDict |> Map.iter (fun _ v -> SaveWordAttr2Stream(v, fs))
            x.DoubleCharDict |> Map.iter (fun _ v -> SaveWordAttr2Stream(v, fs))
            x.WordDict |> Map.iter (fun _ v -> SaveWordAttr2Stream(v, fs)))

    member x.GetWordAttr (word : string) =
        match word.Length with
        | l when l = 1 -> 
            let c = word.ToLower().[0]
            match x.FirstCharDict.ContainsKey c with
            | true -> Some x.FirstCharDict.[c]
            | _ -> None
        | k when k = 2 ->
            let w = word.ToLower()
            let dc = (uint32 w.[0] <<< 16) + (uint32 w.[1])
            match x.DoubleCharDict.ContainsKey dc with
            | true -> Some x.DoubleCharDict.[dc]
            | _ -> None
        | _ -> 
            let wd = word.ToLower()
            match x.WordDict.ContainsKey wd with
            | true -> Some x.WordDict.[wd]
            | _ -> None




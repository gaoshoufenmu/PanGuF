#light

[<AutoOpen>]
module Framework

open System
open System.Text
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Threading
open System.Diagnostics
open System.Xml.Serialization
open System.Collections.Generic
open Microsoft.FSharp.Collections

open System.Text.RegularExpressions

/// Active match pattern
let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern) in
    if m.Success then Some (List.tail [ for g in m.Groups -> g.Value]) else None

let ContainUrl value =
    match value with
    | Match "(http:\/\/\S+)" result -> Some(result.Head)
    | _ -> None

type AppendList<'T>() =
    let DefaultCapacity = 4

    static member private IsCompatibleObject (value : obj) =
        if((value :? 'T) || (value = null && not typedefof<'T>.IsValueType))
        then true
        else false

[<AutoOpen>]
module File =
    /// The process cannot access the file because it is being used by another process
    let ERR_PROCESS_CANNOT_ACCESS_FILE = 0x80070020

    /// Get file length
    let GetFileLength fileName =
        let fileInfo = new FileInfo(fileName)
        fileInfo.Length

    let ReadFileToString fileName (encode: Encoding) =
        match File.Exists fileName with
        | false -> let name = Path.GetFileName fileName
                   let assembly = Assembly.GetExecutingAssembly()
                   let assemblyName = assembly.GetName().Name
                   using (assembly.GetManifestResourceStream(assemblyName + ".Resources." + name)) (fun x ->
                       match x with
                       | null -> None
                       | _    -> using (new StreamReader(x, Encoding.UTF8)) (fun y -> Some(y.ReadToEnd())))
        | true -> let mutable count = 0;
                  let rec ReadStream() = 
                      try
                          using (new FileStream(fileName, FileMode.Open, FileAccess.Read))(fun x ->
                              using (new StreamReader(x, encode))(fun y -> Some(y.ReadToEnd())))
                      with
                          | :? IOException as ioe -> match Marshal.GetHRForException(ioe) with
                                                     | r when r = ERR_PROCESS_CANNOT_ACCESS_FILE  ->
                                                         if(count > 10) 
                                                         then 
                                                             failwith ioe.Message
                                                         else
                                                             Thread.Sleep 200
                                                             count <- count + 1
                                                             ReadStream()
                                                     | _ -> failwith ioe.Message
                  ReadStream()

    /// Read content from a given filestream to memorystream one segment by one segment
    let rec File2Memory (ms: MemoryStream, fs: FileStream) =
        let bytes = Array.zeroCreate<byte>(32768)
        let len = fs.Read(bytes, 0, bytes.Length)
        match len > 0 with
        | true -> ms.Write(bytes, 0, len)
                  File2Memory(ms, fs)
        | false -> fs.Close()
                   ms

    /// Read content from a given file and return a memorystream
    let ReadFileToStream fileName = 
        match File.Exists fileName with
        | false -> let name = Path.GetFileName fileName
                   let assembly = Assembly.GetExecutingAssembly()
                   let stream = assembly.GetName().Name + ".Resources" + name |> assembly.GetManifestResourceStream
                   stream :?> MemoryStream

        | true  -> let mutable count = 0;
                   let rec ReadStream() = 
                       let fs = new FileStream(fileName, FileMode.Open, FileAccess.Read)
                       let ms = new MemoryStream()
                       try
                           File2Memory(ms, fs)
                       with
                           | :? IOException as ioe -> match Marshal.GetHRForException(ioe) with
                                                        | r when r = ERR_PROCESS_CANNOT_ACCESS_FILE  ->
                                                            if(count > 10) 
                                                            then 
                                                                failwith ioe.Message
                                                            else
                                                                Thread.Sleep 200
                                                                count <- count + 1
                                                                ReadStream()
                                                        | _ -> failwith ioe.Message
                   ReadStream()
    
    let ReadResourceToStream name =
        let assembly = Assembly.GetExecutingAssembly()
        let assemblyName = assembly.GetName().Name
        sprintf "%s.Resources.%s" assemblyName name |> assembly.GetManifestResourceStream

    /// Write content from MemoryStream to a given file
    let WriteStream fileName (ms: MemoryStream) =
        let mutable count = 0   
        let rec Write() =
            try
                if(File.Exists fileName) then File.Delete(fileName) else ()
                let fs = new FileStream(fileName, FileMode.CreateNew)
                ms.WriteTo fs
                fs.Close()
            with
                | :? IOException as ioe -> match Marshal.GetHRForException(ioe) with
                                            | r when r = ERR_PROCESS_CANNOT_ACCESS_FILE  ->
                                                if(count > 10) 
                                                then 
                                                    failwith ioe.Message
                                                else
                                                    Thread.Sleep 200
                                                    count <- count + 1
                                                    Write()
                                            | _ -> failwith ioe.Message
        Write()

    /// Write a string into a given file as a line content      
    let WriteLine fileName =
        let mutable count = 0
        let mutable line = "";
        let rec Write() =
            try
                using (new FileStream(fileName, FileMode.Append))(fun x ->
                    using(new StreamWriter(x, Encoding.UTF8))(fun y -> 
                        y.WriteLine(line)
                        line))
            with
                | :? IOException as ioe -> match Marshal.GetHRForException(ioe) with
                                            | r when r = ERR_PROCESS_CANNOT_ACCESS_FILE  ->
                                                if(count > 10) 
                                                then 
                                                    failwith ioe.Message
                                                else
                                                    Thread.Sleep 200
                                                    count <- count + 1
                                                    Write()
                                            | _ -> failwith ioe.Message
        Write()

    /// Write a string into a given file
    let WriteString fileName (str:string) (encode: Encoding) =
        let mutable count = 0
        let rec Write() =
            try
                if(File.Exists fileName) then File.Delete(fileName) else ()
                let fs = new FileStream(fileName, FileMode.CreateNew)
                let sw = new StreamWriter(fs, encode)
                sw.Write str
                sw.Close()
                fs.Close()
            with
                | :? IOException as ioe -> match Marshal.GetHRForException(ioe) with
                                            | r when r = ERR_PROCESS_CANNOT_ACCESS_FILE  ->
                                                if(count > 10) 
                                                then 
                                                    failwith ioe.Message
                                                else
                                                    Thread.Sleep 200
                                                    count <- count + 1
                                                    Write()
                                            | _ -> failwith ioe.Message
        Write()

    let rec DeleteFile (path: string) fileName ``recursive`` =
        let stdPath = if(path.EndsWith("\\")) then path else path + "\\"
        match ``recursive`` with
        | false -> File.Delete(stdPath + fileName)
        | true  -> File.Delete(stdPath + fileName)
                   let items = Directory.GetDirectories path
                   for item in items do
                       DeleteFile item fileName true


[<AutoOpen>]
module Regex =
    /// Retrieve matched item in a given group collection expect the first matched item
    let rec RetrieveGroupCollection index (groups: GroupCollection) =
        match index < groups.Count with
        | true -> groups.[index].Value.Trim() :: RetrieveGroupCollection (index + 1) groups
        | _ -> []    
    
    /// Retrieve matched item in a given match collection
    let rec RetrieveMatchCollection index (mc: MatchCollection) =
        match index < mc.Count with
        | true -> (RetrieveGroupCollection 1 mc.[index].Groups) @ RetrieveMatchCollection (index + 1) mc
        | _ -> []

    let rec GetMatch (text: string) (regex: string) ignoreCase =
        // Get the last match item
        let TailMatch text regex ignoreCase =
            let reg = match ignoreCase with
                      | true -> new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
                      | _    -> new Regex(regex, RegexOptions.Singleline)
            let m = reg.Match text
            match m.Groups.Count > 0 with
            | true -> m.Groups.[m.Groups.Count - 1].Value
            | _    -> ""
                    
        let index = regex.IndexOf "(.+)"
        let begin_ = 
            match index < 0 with
            | true ->
                let index_ = regex.IndexOf "(.+?)"
                if (index_ >= 0) then index_ + 5 else 0
            | _    -> index + 4
        
        // if regex contains "(.+)" or "(.+?)", then it should check if text match the regex's tail
        // e.g. regex = "(.+)[a|bc]", firstly, check if text matches "[a|bc]", if not ,returns "", otherwise regex.Match text
        match begin_ >= 0 with
        | true -> 
            let tail = regex.Substring begin_
            match tail <> "" with
            | true -> 
                let r = GetMatch text tail ignoreCase
                if(r = "") then "" else TailMatch text regex ignoreCase
            | _    -> TailMatch text regex ignoreCase
        | _ -> TailMatch text regex ignoreCase

    let GetMatchStrings (text: string) (regex: string) ignoreCase =
        let GetMatchR text regex ignoreCase =
            let reg = 
                match ignoreCase with
                | true -> new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
                | _    -> new Regex(regex, RegexOptions.Singleline)
            let ms = reg.Matches text
            match ms.Count = 0 with
            | true -> (false, List.empty<string>)
            | _ ->
                let strs = RetrieveMatchCollection 0 ms
                (true, strs)

        let index = regex.IndexOf("(.+)")
        let ``begin`` = match index < 0 with
                        | true ->
                            let index_ = regex.IndexOf("(.+?)")
                            if (index_ >= 0) then index_ + 5 else 0
                        | _ -> index + 4
        
        match ``begin`` >= 0 with
        | true -> 
            let tail = regex.Substring ``begin``
            let m_ = GetMatch text tail ignoreCase
            match m_ <> "" with
            | true -> (false, List.empty<string>)
            | _ -> GetMatchR text regex ignoreCase
        | _ -> GetMatchR text regex ignoreCase

    let Split delimiter input = input |> (new Regex(delimiter)).Split

    let SplitWithOption input delimiter option = input |> (new Regex(delimiter, option)).Split

    let Replace text regex (newText:string) ignoreCase =
        let reg =
            match ignoreCase with
            | true -> new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
            | _    -> new Regex(regex, RegexOptions.Singleline)
        reg.Replace(text, newText)

    let GetSingleMatchStrings text (regex: string) ignoreCase =
        let GetMatchR text regex ignoreCase =
            let reg = 
                match ignoreCase with
                | true -> new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
                | _    -> new Regex(regex, RegexOptions.Singleline)
            let ms = reg.Matches text
            match ms.Count = 0 with
            | true -> (false, List.empty<string>)
            | _ ->
                let strs = 
                    seq { for m in ms do
                              let count = m.Groups.Count
                              let item =
                                  match count > 0 with
                                  | true -> Some(m.Groups.[count - 1].Value.Trim())
                                  | _ -> None
                              yield item
                    } |> Seq.where (fun x -> x.IsSome) |> Seq.map (fun x -> x.Value) |> List.ofSeq
                (true, strs)
        let index = regex.IndexOf "(.+)"
        let ``begin`` =
            match index < 0 with
            | true ->
                let index_ = regex.IndexOf "(.+?)"
                if (index_ >= 0) then index_ + 5 else 0
            | _ -> index + 4

        match ``begin`` >= 0 with
        | true ->
            let tail = regex.Substring ``begin``
            let m_ = GetMatch text tail ignoreCase
            match m_ = "" with
            | true -> (false, List.empty<string>)
            | _ -> GetMatchR text regex ignoreCase
        | _ -> GetMatchR text regex ignoreCase

    let GetSplitWithoutFirstStrings text regex ignoreCase =
        let reg =
            match ignoreCase with
            | true -> new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
            | _ -> new Regex(regex, RegexOptions.Singleline)

        let ss = reg.Split text
        match ss.Length < 2 with
        | true -> (false, List.empty<string>)
        | _ ->
            let ``ss`` = ss |> List.ofArray |> List.tail
            (true, ``ss``)

    /// Get all matched strings and concat them into a single string
    let GetMatch2Con text (regex: string) ignoreCase =
        let GetMatchS text regex ignoreCase =
            let option =
                if ignoreCase then RegexOptions.IgnoreCase ||| RegexOptions.Singleline else RegexOptions.Singleline
            let ms = (new Regex(regex, option)).Match text
            let rec ConcatMatchString index (groups: GroupCollection) =
                match index < groups.Count with
                | true -> groups.[index].Value + ConcatMatchString (index + 1) groups
                | _ -> ""
            ConcatMatchString 1 ms.Groups

        let index = regex.IndexOf "(.+)"
        let ``begin`` =
            match index < 0 with
            | true -> 
                let index_ = regex.IndexOf "(.+?)"
                if(index_ < 0) then 0 else index_ + 5
            | _ -> index + 4
        match ``begin`` >= 0 with
        | true ->
            let tail = regex.Substring ``begin``
            match GetMatch text tail ignoreCase = "" with
            | true -> ""
            | _ -> GetMatchS text regex ignoreCase
        | _ -> GetMatchS text regex ignoreCase


    let ``ch`` c (s: string) = seq { if s.Length > 0 && s.[0] = c then yield s.Substring 1 }
    let (=>) l r s = seq { for sl in l s do for sr in r sl -> sr }
    let (<|>) l r s = seq { yield! l s; yield! r s}
    let rec (<*>) e s = seq { yield s; yield! (e => (<*>) e) s }
    let (<+>) e = e => (<*>) e

    // example c(a|d)+r
    let pattern = ``ch`` 'c' => (<+>) (``ch`` 'a' <|> ``ch`` 'd') => ``ch`` 'r'

[<AutoOpen>]
module Path =

    let GetAssemblyPath() =
        let _PREFIX = @"file:///"
        let codeBase = Assembly.GetExecutingAssembly().CodeBase
        let codeBase_ = codeBase.Substring(_PREFIX.Length, codeBase.Length - _PREFIX.Length).Replace("/", "\\")
        Path.GetDirectoryName(codeBase) + @"\"

    let ProcessDirectory =
        let curFileName = Process.GetCurrentProcess().MainModule.FileName
        Path.GetDirectoryName(curFileName)

    let AppendDivision path (division: char) =
        match path with
        | i when (i = "" || ((i.[i.Length - 1] <> '\\') && (i.[i.Length - 1] <> '/'))) -> i + division.ToString()
        | _ -> if(path.[path.Length - 1] <> division) then path.Substring(0, path.Length - 1) + division.ToString() else path

    let GetFolderName path =
        let path_ = (Path.GetFullPath path).TrimEnd('\\')
        let index = path_.LastIndexOf '\\'
        path_.Substring(index + 1)

[<AutoOpen>]
module XmlSerialization =
    let Serialize2Stream obj encode (s: Stream) =
        let ser = new XmlSerializer(obj.GetType())
        let w = new StreamWriter(s, encode)
        ser.Serialize(w, obj)
        s

    let Serialize2Mem obj encode =
        let s = new MemoryStream()
        Serialize2Stream obj encode s |> ignore
        s.Position <- 0L
        s

    let Serialize obj = Serialize2Mem obj Encoding.UTF8

    let Deserialize (stream: Stream) (objType: Type) =
        stream.Position <- 0L
        let ser = new XmlSerializer(objType)
        ser.Deserialize stream

    let GSerialize3<'T> (obj : 'T) (encode: Encoding) (s : Stream) =
        let writer = new StreamWriter(s, encode)
        let ser = new XmlSerializer(typedefof<'T>)
        ser.Serialize(writer, obj)
        s

    let GSerialize2<'T> (obj : 'T) (encode : Encoding) =
        let s = new MemoryStream()
        GSerialize3(obj, encode, s) |> ignore
        s

    let GSerialize1<'T> (obj : 'T) = GSerialize2 obj Encoding.UTF8

    let GDeserialize<'T> (stream : Stream) =
        stream.Position <- 0L
        let ser = new XmlSerializer(typedefof<'T>)
        downcast ser.Deserialize(stream) : 'T

    


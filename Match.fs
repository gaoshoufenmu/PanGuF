#light
[<AutoOpen>]
module Match

open System
open System.Reflection

[<Serializable>]
type MatchOptions() =
    member val ChineseNameIdentify         = false with get, set     // 中文人名识别
    member val FrequencyFirst              = false with get, set     // 词频优先
    member val MultiDimensionality         = true with get, set      // 多元分词
    member val EnglishMultiDimensionality  = false with get, set     // 英文多元分词，可将字母和数字分开
    member val FilterStopWords             = true with get, set      // 过滤停用词
    member val IgnoreSpace                 = true with get, set      // 忽略空白
    member val FoceSingleWord              = false with get, set     // 强制一元分词
    member val TraditionalChineseEnabled   = false with get, set     // 繁体开关
    member val OutputSimplifiedTraditional = false with get, set     // 同时输出简体和繁体
    member val UnknownWordIdentify         = true with get, set      // 未登录词识别
    member val FilterEnglish               = false with get, set     // 过滤英文，只在过滤停用词选项生效时有效
    member val FilterNumeric               = false with get, set     // 过滤数字，只在过滤停用词选项生效时有效
    member val IgnoreCapital               = false with get, set     // 忽略英文大小写
    member val EnglishSegment              = false with get, set     // 英文分词
    member val SynonymOutput               = false with get, set     // 同义词输出，一般用于对搜索字符串分词，不建议在索引时使用
    member val WildcardOutput              = false with get, set     // 通配符匹配输出，一般用于对搜索字符串分词，不建议在索引时使用
    member val WildcardSegment             = false with get, set     // 对通配符匹配的结果分词
    member val CustomRule                  = false with get, set     // 是否用户自定义规则匹配

    member this.Clone() : MatchOptions =
        let result = new MatchOptions()
        for f in result.GetType().GetFields() do
            f.SetValue(result, f.GetValue(this))
        result

[<Serializable>]
type MatchParameter() =
    let mutable _redundancy = 0

    member this.Redundancy 
        with get() = _redundancy
        and  set y = match y with
                     | i when i < 0 -> _redundancy <- 0
                     | j when j >= 3 -> _redundancy <- 2
                     | _ -> _redundancy <- y

    member this.UnKnowRank = 1  // 未登录词权值
    member this.BestRank   = 5  // 最匹配词权值
    member this.SecRank    = 3  // 次匹配词权值
    member this.ThirdRank  = 2  // 再匹配词权值
    member this.SingleRank = 1  // 强行输出的单字的权值
    member this.SymbolRank = 1  // 符号权值
    member this.SynonymRank         = 1     // 同义词权值
    member this.WildcardRank        = 1     // 通配符匹配结果权值
    member this.NumericRank         = 1     // 数字的权值
    member this.EnglishRank         = 5     // 英文词汇权值
    member this.EnglishLowerRank    = 3     // 英文词汇小写的权值
    member this.EnglishStemRank     = 2     // 英文词汇词根的权值
    member this.FilterEnglishLength = 0     // 过滤英文选项生效时，过滤大于这个长度的英文
    member this.FilterNumericLength = 0     // 过滤数字选项生效时，过滤大于这个长度的数字
    member this.CustomRuleAssemblyFileName  = ""    // 用户自定义规则的配件文件名
    member this.CustomRuleFullClassName     = ""    // 用户自定义规则的类的完整名，即带名字空间的名称
    member this.SimplifiedTraditionalRank   = 1     // 强制同时输出简繁汉字时，非原来文本的汉字输出权值。比如原来文本是简体，这里就是输出的繁体字的权值，反之亦然

    member this.Clone() =
        let param = new MatchParameter()
        for f in this.GetType().GetFields() do
            let v = f.GetValue(this)
            f.SetValue(param, v)
        param

type IChsFullTextMatch = 
    interface
        abstract Options: MatchOptions
        abstract Parameters : MatchParameter

    end

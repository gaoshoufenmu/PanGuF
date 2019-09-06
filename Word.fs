#light
[<AutoOpen>]
module Word

open System

type WordInfo (word: string, position: int, pos: POS, frequency: double, 
                rank: int, wordType: WordType, originWordType: WordType) =
    inherit WordAttribute(word, pos, frequency)
    member this.WordType = wordType
    member this.OrginWordType = originWordType
    member this.Position = position
    member this.Rank = rank         // 单词权重

    new () = new WordInfo("", 0, POS.POS_D_A, 0.0, 0, WordType.None, WordType.None)
    new (word, pos, frequency) = new WordInfo(word, 0, pos, frequency, 0, WordType.None, WordType.None)
    new (wa: WordAttribute) = new WordInfo(wa.Word, 0, wa.Pos, wa.Frequency, 0, WordType.None, WordType.None)
    new (pl: PositionLength, originText: string, param: MatchParameter) = 
        let word = originText.Substring(pl.Position, pl.Length)
        let rank = 
            match pl.Level with
            | 1 -> param.SecRank
            | 2 -> param.ThirdRank
            | 3 -> param.SingleRank
            | _ -> param.BestRank
        new WordInfo(word, pl.Position, pl.WordAttr.Pos, pl.WordAttr.Frequency, rank, WordType.SimplifiedChinese, WordType.None)

    member this.GetEndPosition() = this.Position + this.Word.Length

    interface IComparable<WordInfo> with
        member this.CompareTo(other: WordInfo) =
            if this.Position <> other.Position
            then this.Position.CompareTo other.Position
            else this.Word.Length.CompareTo other.Word.Length




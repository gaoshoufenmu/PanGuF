﻿[<AutoOpen>]
module WordAttr

open System

type WordType =  None = 0
               | English =1
               | SimplifiedChinese = 2
               | TraditionalChinese = 3
               | Numeric = 4
               | Symbol  = 5
               | Space   = 6
               | Synonym = 7

[<Flags>]
type POS =  POS_D_A = 0x40000000    // 形容词 形语素
          | POS_D_B = 0x20000000    // 区别词 区别语素
          | POS_D_C = 0x10000000    // 连词
          | POS_D_D = 0x08000000    // 副词
          | POS_D_E = 0x04000000    // 叹词
          | POS_D_F = 0x02000000    // 方位词
          | POS_D_I = 0x01000000    // 成语
          | POS_D_L = 0x00800000    // 习语
          | POS_A_M = 0x00400000    // 数词
          | POS_D_MQ = 0x00200000   // 数量词
          | POS_D_N = 0x00100000    // 名词
          | POS_D_O = 0x00080000    // 拟声词
          | POS_D_P = 0x00040000    // 介词
          | POS_A_Q = 0x00020000    // 量词
          | POS_D_R = 0x00010000    // 代词
          | POS_D_S = 0x00008000    // 处所词
          | POS_D_T = 0x00004000    // 时间词
          | POS_D_U = 0x00002000    // 助词
          | POS_D_V = 0x00001000    // 动词
          | POS_D_W = 0x00000800    // 标点符号
          | POS_D_X = 0x00000400    // 非语素字
          | POS_D_Y = 0x00000200    // 语气词
          | POS_D_Z = 0x00000100    // 状态词
          | POS_A_NR = 0x00000080   // 人名
          | POS_A_NS = 0x00000040   // 地名
          | POS_A_NT = 0x00000020   // 机构团体
          | POS_A_NX = 0x00000010   // 外文字符
          | POS_A_NZ = 0x00000008   // 其他专名
          | POS_D_H = 0x00000004    // 前接成分
          | POS_D_K = 0x00000002    // 后接成分
          | POS_UNK = 0x00000000    // 未知词性
            


type WordAttribute(word: string, pos: POS, frequency: double) =
    /// word
    member this.Word = word
    /// part of speech
    member this.Pos = pos
    /// frequency for this word
    member this.Frequency = frequency

    new () = new WordAttribute ("", POS.POS_D_A, 0.0)

    override this.ToString() = this.Word

    interface IComparable<WordAttribute> with
        override this.CompareTo other = this.Word.CompareTo other.Word

[<Serializable>]
type WordAttributeStruct =
    struct
        val Word: string
        val Pos : POS
        val Frequency : double
        new (wa : WordAttribute) = { Word = wa.Word; Pos = wa.Pos; Frequency = wa.Frequency }
        new (word, pos, frequency) = { Word = word; Pos = pos; Frequency = frequency }
        override x.ToString() = x.Word
    end




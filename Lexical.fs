#light
[<AutoOpen>]
module Lexical

type LexicalFunction = None = 0
                     | OutputIdentifier = 1
                     | DoSpace = 2
                     | OutputSpace = 3
                     | OutputNumeric = 4
                     | OutputChinese = 5
                     | Other = 255


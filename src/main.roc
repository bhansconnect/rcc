app "rcc"
    packages { pf: "../platform/main.roc" }
    imports [pf.Task.{ Task, await }, pf.Stderr, pf.File]
    provides [main] to pf


# So my plan for actual preprocessing (no idea if it will work in practice) is
#  1. load entire file
#  2. copy to a new buffer removing backslash newlines as you go (add some debug info around lines merged)
#  3. tokenize into preprocessor tokens (include line #, file, and kind)
#  4. generate an output list of tokens based off of the input tokens (will require processing macros and running step 1 to 2 on other files)
#  5. either dump to file from the final token list or pass the token list to a stricter c parser.

# With this design, we will never generate a final preprocessed text file.
# Instead we will just generate a giant list of references to the many input text files.
# This is willingly and explicitly using a few large buffers.

# Extra note, in a lot of cases, I am trying to write more idiomatic and nice Roc code.
# This may cost too much perf and need to be changed in the future.
# That said, I am trying to make more DOD friendly data structures.


main : Str -> Task {} []
main = \filename ->
    bytes <- File.readBytes filename |> await
    (mergedBytes, mergeIndicies) = removeBackslashNewlines bytes

    preprocessTokenize mergedBytes 0
    |> okOrCrash "failed to tokenize"
    |> List.map (\tok -> debugDisplayPPToken tok mergedBytes mergeIndicies)
    |> Str.joinWith "\n"
    |> Str.toUtf8
    |> Stderr.raw

# We are limitting the offset to a U32 and the file number to a U8.
# This means at most 2GiB of source file and 256 included files is allowed.
# These limits may be too small, but we shall see.
PPToken : {offset: U32, fileNum: U8, kind: PPKind}

# The kinds of preprocessor tokens.
PPKind : [
    Identifier,
    HeaderName,
    StringLiteral,
    Number,
    CharacterConstant,

    LBracket,
    RBracket,
    LParen,
    RParen,
    LSquiggle,
    RSquiggle,
    Dot,
    Arrow,
    Inc,
    Dec,
    BitAnd,
    Mul,
    Add,
    Sub,
    BitNot,
    Not,
    Div,
    Mod,
    LShift,
    RShift,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Ne,
    Xor,
    BitOr,
    And,
    Or,
    QuestionMark,
    Colon,
    Semicolon,
    DotDotDot,
    Assign,
    MulAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    LShiftAssign,
    RShiftAssign,
    BitAndAssign,
    XorAssign,
    BitOrAssign,
    Comma,
    HashHash,

    # TODO: add specific preprocessor tokens.
    # Also a a general hash line for others.


    # Is this really needed? "each non-white-space character that cannot be one of the above"
    Other
]

debugDisplayPPToken = \{fileNum, offset, kind}, mergedBytes, mergeIndicies ->
    fileNumStr = Num.toStr fileNum
    # This is hugely wasteful because it is done per token.
    # Really this can be cached.
    (line, col) = calculateLineCol mergedBytes mergeIndicies offset
    lineStr = Num.toStr line
    colStr = Num.toStr col

    kindStr =
        when kind is
            Identifier -> "Identifier"
            HeaderName -> "HeaderName"
            StringLiteral -> "StringLiteral"
            Number -> "Number"
            CharacterConstant -> "CharacterConstant"
            LBracket -> "LBracket"
            RBracket -> "RBracket"
            LParen -> "LParen"
            RParen -> "RParen"
            LSquiggle -> "LSquiggle"
            RSquiggle -> "RSquiggle"
            Dot -> "Dot"
            Arrow-> "Arrow"
            Inc -> "Inc"
            Dec -> "Dec"
            BitAnd -> "BitAnd"
            Mul -> "Mul"
            Add -> "Add"
            Sub -> "Sub"
            BitNot -> "BitNot"
            Not -> "Not"
            Div -> "Div"
            Mod -> "Mod"
            LShift -> "LShift"
            RShift -> "RShift"
            Lt -> "Lt"
            Gt -> "Gt"
            Lte -> "Lte"
            Gte -> "Gte"
            Eq -> "Eq"
            Ne -> "Ne"
            Xor -> "Xor"
            BitOr -> "BitOr"
            And -> "And"
            Or -> "Or"
            QuestionMark -> "QuestionMark"
            Colon -> "Colon"
            Semicolon -> "Semicolon"
            DotDotDot -> "DotDotDot"
            Assign -> "Assign"
            MulAssign -> "MulAssign"
            DivAssign -> "DivAssign"
            ModAssign -> "ModAssign"
            AddAssign -> "AddAssign"
            SubAssign -> "SubAssign"
            LShiftAssign -> "LShiftAssign"
            RShiftAssign -> "RShiftAssign"
            BitAndAssign -> "BitAndAssign"
            XorAssign -> "XorAssign"
            BitOrAssign -> "BitOrAssign"
            Comma -> "Comma"
            HashHash -> "HashHash"
            Other -> "Other"

    "{ file: \(fileNumStr), line: \(lineStr), col: \(colStr), kind: \(kindStr) }"

calculateLineCol = \bytes, mergeIndicies, offset ->
    extra = List.countIf mergeIndicies (\x -> x <= offset)
    {before} = List.split bytes (Num.toNat offset)
    line = (List.countIf before (\x -> x == '\n')) + extra + 1
    col = List.walkBackwardsUntil before 1 \count, elem ->
        if elem == '\n' then
            Break count
        else
            Continue (count + 1)
    (line, col)

# Converts a List of Bytes into a List of C preprocessing tokens.
# The tokens will still references the original list of bytes.
# This function expects backslash newlines to have already been dealt with.
# This also requires the file index for error messages and debugging purposes.
preprocessTokenize : List U8, U8 -> Result (List PPToken) _
preprocessTokenize = \bytes, fileNum ->
    preprocessTokenizeHelper bytes [] 0 fileNum

preprocessTokenizeHelper : List U8, List PPToken, Nat, U8 -> Result (List PPToken) _
preprocessTokenizeHelper = \bytes, tokens, offset, fileNum ->
    cleanedOffset = consumeCommentsAndWhitespace bytes offset
    # Wild guess, the perf of this will be pretty bad and I will need to do it a different way.
    when List.drop bytes cleanedOffset is
        ['%', ':', '%', ':', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum HashHash 4
        ['.', '.', '.', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum DotDotDot 3
        ['<', '<', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum LShiftAssign 3
        ['>', '>', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum RShiftAssign 3
        ['-', '>', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Arrow 2
        ['+', '+', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Inc 2
        ['-', '-', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Dec 2
        ['<', '<', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum LShift 2
        ['>', '>', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum RShift 2
        ['<', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Lte 2
        ['>', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Gte 2
        ['=', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Eq 2
        ['!', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Ne 2
        ['&', '&', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum And 2
        ['|', '|', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Or 2
        ['*', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum MulAssign 2
        ['/', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum DivAssign 2
        ['%', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum ModAssign 2
        ['+', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum AddAssign 2
        ['-', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum SubAssign 2
        ['&', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum BitAndAssign 2
        ['^', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum XorAssign 2
        ['|', '=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum BitOrAssign 2
        ['#', '#', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum HashHash 2
        ['<', ':', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum LBracket 2
        [':', '>', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum RBracket 2
        ['<', '%', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum LSquiggle 2
        ['%', '>', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum RSquiggle 2
        ['[', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum LBracket 1
        [']', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum RBracket 1
        ['(', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum LParen 1
        [')', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum RParen 1
        ['{', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum LSquiggle 1
        ['}', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum RSquiggle 1
        ['.', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Dot 1
        ['&', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum BitAnd 1
        ['*', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Mul 1
        ['+', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Add 1
        ['-', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Sub 1
        ['~', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum BitNot 1
        ['!', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Not 1
        ['/', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Div 1
        ['%', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Mod 1
        ['<', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Lt 1
        ['>', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Gt 1
        ['^', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Xor 1
        ['|', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum BitOr 1
        ['?', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum QuestionMark 1
        [':', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Colon 1
        [';', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Semicolon 1
        ['=', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Assign 1
        [',', ..] ->
            addPunctuator bytes tokens cleanedOffset fileNum Comma 1
        [x, ..] ->
            if isIdentifierNonDigit x then
                nextTokens = List.append tokens { fileNum, offset: Num.toU32 cleanedOffset, kind: Identifier }

                nextOffset = consumeIdentifier bytes cleanedOffset
                preprocessTokenizeHelper bytes nextTokens nextOffset fileNum
            else
                Err (UnexpectedCharacter x)
        # [x, ..] if isIdentifierNonDigit x ->
        # [x, ..] ->
        #     Err (UnexpectedCharacter x)
        [] ->
            Ok tokens

addPunctuator : List U8, List PPToken, Nat, U8, PPKind, Nat -> Result (List PPToken) _
addPunctuator = \bytes, tokens, offset, fileNum, kind, size ->
    nextTokens = List.append tokens { fileNum, offset: Num.toU32 offset, kind }

    preprocessTokenizeHelper bytes nextTokens (offset + size) fileNum

consumeCommentsAndWhitespace = \bytes, offset ->
    when List.drop bytes offset is
        ['/', '/', ..] ->
            nextOffset = consumeRestOfLine bytes (offset + 2)
            consumeCommentsAndWhitespace bytes nextOffset
        ['/', '*', ..] ->
            nextOffset = consumeMultilineComment bytes (offset + 2)
            consumeCommentsAndWhitespace bytes nextOffset
        # [x, ..] if isWhitespace x -> Some reason this is breaking alias analysis
        [x, ..] ->
            if isWhitespace x then
                consumeCommentsAndWhitespace bytes (offset + 1)
            else
                offset
        _ ->
            offset

consumeMultilineComment = \bytes, offset ->
    when List.drop bytes offset is
        ['*', '/', ..] ->
            offset + 2
        [_, ..] ->
            consumeMultilineComment bytes (offset + 1)
        [] ->
            crash "file ended in the middle of a multi-line comment"

consumeRestOfLine = \bytes, offset ->
    when List.get bytes offset is
        Ok x if x != '\n' ->
            consumeRestOfLine bytes (offset + 1)
        _ ->
            offset

isWhitespace = \x ->
    (x >= '\t' && x <= '\r') || (x == ' ')

consumeIdentifier = \bytes, offset ->
    when List.get bytes offset is
        Ok x if isIdentifier x ->
            consumeIdentifier bytes (offset + 1)
        _ ->
            offset

isIdentifier = \x ->
    (isIdentifierNonDigit x) || (x >= '0' && x <= '9')

isIdentifierNonDigit = \x ->
    (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || x == '_'

# Removes all cases of backslash followed by newline.
removeBackslashNewlines : List U8 -> (List U8, List U32)
removeBackslashNewlines = \src ->
    helper = \in, out, mi ->
        when in is
            [b0, b1, b2, b3, b4, b5, b6, b7, ..] ->
                b76 = Num.bitwiseOr (b7 |> Num.toU64 |> Num.shiftLeftBy 56) (b6 |> Num.toU64 |> Num.shiftLeftBy 48)
                b54 = Num.bitwiseOr (b5 |> Num.toU64 |> Num.shiftLeftBy 40) (b4 |> Num.toU64 |> Num.shiftLeftBy 32)
                b32 = Num.bitwiseOr (b3 |> Num.toU64 |> Num.shiftLeftBy 24) (b2 |> Num.toU64 |> Num.shiftLeftBy 16)
                b10 = Num.bitwiseOr (b1 |> Num.toU64 |> Num.shiftLeftBy 8) (b0 |> Num.toU64)
                b7654 = Num.bitwiseOr b76 b54
                b3210 = Num.bitwiseOr b32 b10

                # SWAR techniques from: http://0x80.pl/notesen/2023-03-06-swar-find-any.html
                swar : U64
                swar = Num.bitwiseOr b7654 b3210

                matchBackslash =
                    swar
                    |> Num.bitwiseXor 0x5C5C5C5C5C5C5C5C # make it so that backslash bytes have a 0 value
                    |> Num.bitwiseAnd 0x7F7F7F7F7F7F7F7F # avoid overflow from utf8
                    |> Num.addWrap 0x7F7F7F7F7F7F7F7F    # Make it so that any value greater than zero has the first bit set.
                    |> Num.bitwiseOr swar                # Make utf8 also have its first bit set
                    |> Num.bitwiseAnd 0x8080808080808080 # keep only match/no match bit
                    |> Num.bitwiseXor 0x8080808080808080 # flip result such that matches are ones


                # The drop call here will be terribly slow until roc has seamless slices.
                # Good test bed to make sure they work correctly.
                if matchBackslash > 0 then
                    # We have at least one backslash.
                    matchIndex = Num.countTrailingZeroBits matchBackslash // 8
                    
                    {before, others} = List.split in matchIndex
                    # Backslash will be the first character of others.

                    nextOut = List.concat out before
                    when others is
                        ['\\', '\r', '\n', ..] ->
                            index = List.len nextOut |> Num.toU32
                            helper (List.drop others 3) nextOut (List.append mi index)
                        ['\\', '\n', ..] ->
                            index = List.len nextOut |> Num.toU32
                            helper (List.drop others 2) nextOut (List.append mi index)
                        ['\\', '\r', ..] ->
                            index = List.len nextOut |> Num.toU32
                            helper (List.drop others 2) nextOut (List.append mi index)
                        _ ->
                            # Not a match, so put the backslash in the output and continue.
                            helper (List.drop others 1) (List.append nextOut '\\') mi
                else
                    nextOut =
                        out
                        |> List.append b0
                        |> List.append b1
                        |> List.append b2
                        |> List.append b3
                        |> List.append b4
                        |> List.append b5
                        |> List.append b6
                        |> List.append b7
                    helper (List.drop in 8) nextOut mi

            _ ->
                (in, out, mi)

    (remaining, cleaned, mergeIndices) = helper src (List.withCapacity (List.len src)) []

    removeBackslashNewlinesRemaining remaining cleaned mergeIndices

removeBackslashNewlinesRemaining = \in, out, mergeIndices ->
    when in is
        ['\\', '\r', '\n', ..] ->
            index = List.len out |> Num.toU32
            removeBackslashNewlinesRemaining (List.drop in 3) out (List.append mergeIndices index)
        ['\\', '\n', ..] ->
            index = List.len out |> Num.toU32
            removeBackslashNewlinesRemaining (List.drop in 2) out (List.append mergeIndices index)
        ['\\', '\r', ..] ->
            index = List.len out |> Num.toU32
            removeBackslashNewlinesRemaining (List.drop in 2) out (List.append mergeIndices index)
        [x, ..] ->
            nextOut = List.append out x
            removeBackslashNewlinesRemaining (List.drop in 1) nextOut mergeIndices
        [] ->
            (out, mergeIndices)


okOrCrash = \res, msg ->
    when res is
        Ok x -> x
        Err (UnexpectedCharacter x)->
            xStr = Num.toStr x
            crash "\(msg): UnexpectedCharacter \(xStr)"
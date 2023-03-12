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


main : Str -> Task {} []
main = \filename ->
    bytes <- File.readBytes filename |> await
    bytes
    |> removeBackslashNewlines
    |> Stderr.raw


# Removes all cases of backslash followed by newline.
removeBackslashNewlines : List U8 -> List U8
removeBackslashNewlines = \src ->
    helper = \in, out ->
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
                            helper (List.drop others 3) nextOut
                        ['\\', '\n', ..] ->
                            helper (List.drop others 2) nextOut
                        _ ->
                            # Not a match, so put the backslash in the output and continue.
                            helper (List.drop others 1) (List.append nextOut '\\')
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
                    helper (List.drop in 8) nextOut

            _ ->
                T in out

    T remaining cleaned = helper src (List.withCapacity (List.len src))

    removeBackslashNewlinesRemaining remaining cleaned

removeBackslashNewlinesRemaining = \in, out ->
    when in is
        ['\\', '\r', '\n', ..] ->
            removeBackslashNewlinesRemaining (List.drop in 3) out
        ['\\', '\n', ..] ->
            removeBackslashNewlinesRemaining (List.drop in 2) out
        [x, ..] ->
            nextOut = List.append out x
            removeBackslashNewlinesRemaining (List.drop in 1) nextOut
        [] ->
            out

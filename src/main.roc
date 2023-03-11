app "rcc"
    packages { pf: "../platform/main.roc" }
    imports [pf.Task.{ Task, await }, pf.Stderr, pf.File]
    provides [main] to pf

main : Str -> Task {} []
main = \filename ->
    bytes <- File.readBytes filename |> await
    when Str.fromUtf8 (mergeBackSlashNewLines bytes) is
        Ok str ->
            Stderr.raw str
        Err _ ->
            crash "so sad"


# Removes all cases of back slash followed by `\n`.

mergeBackSlashNewLines : List U8 -> List U8
mergeBackSlashNewLines = \src ->
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

                matchBackSlash =
                    swar
                    |> Num.bitwiseXor 0x5C5C5C5C5C5C5C5C # make it so that back slash bytes have a 0 value
                    |> Num.bitwiseAnd 0x7F7F7F7F7F7F7F7F # avoid overflow from utf8
                    |> Num.addWrap 0x7F7F7F7F7F7F7F7F    # Make it so that any value greater than zero has the first bit set.
                    |> Num.bitwiseOr swar                # Make utf8 also have its first bit set
                    |> Num.bitwiseAnd 0x8080808080808080 # keep only match/no match bit
                    |> Num.bitwiseXor 0x8080808080808080 # flip result such that matches are ones
                    |> Num.isGt 0                        # we have a match


                nextOut =
                    if matchBackSlash then
                        # TODO: actually check if the backslash if follwoed by a new line.
                        # in doing so we can look pasth b7 if need.
                        # push everything else to out and make sure to drop extra elements from in if needed.
                        out
                    else
                        out
                        |> List.append b0
                        |> List.append b1
                        |> List.append b2
                        |> List.append b3
                        |> List.append b4
                        |> List.append b5
                        |> List.append b6
                        |> List.append b7

                # The drop call here will be terribly slow until roc has seamless slices.
                # Good test bed to make sure they work correctly.
                helper (List.drop in 8) nextOut
            _ ->
                T in out

    T remaining cleaned = helper src (List.withCapacity (List.len src))

    # TODO check the last upto 7 bytes in remaining for merging/joining.

    cleaned
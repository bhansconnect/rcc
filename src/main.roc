app "rcc"
    packages { pf: "../platform/main.roc" }
    imports [pf.Task.{ Task, await }, pf.Stderr, pf.File]
    provides [main] to pf

main : Str -> Task {} []
main = \filename ->
    bytes <- File.readBytes filename |> await
    when Str.fromUtf8 (removeBackSlashNewLines bytes) is
        Ok str ->
            Stderr.raw str
        Err _ ->
            crash "so sad"


# Removes all cases of back slash followed by `\n`.
removeBackSlashNewLines : List U8 -> List U8
removeBackSlashNewLines = \src ->
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


                # The drop call here will be terribly slow until roc has seamless slices.
                # Good test bed to make sure they work correctly.
                if matchBackSlash > 0 then
                    # We have at least one backslash.
                    # Do slow terrible very explicit fall back
                    # TODO: make this reasonable. Preferably get the first back slash from the match and just check it.
                    when in is
                        ['\\', '\n', ..] ->
                            helper (List.drop in 2) out
                        ['\\', '\r', '\n', ..] ->
                            helper (List.drop in 3) out
                        [x0, '\\', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                            helper (List.drop in 3) nextOut
                        [x0, '\\', '\r', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                            helper (List.drop in 4) nextOut
                        [x0, x1, '\\', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                            helper (List.drop in 4) nextOut
                        [x0, x1, '\\', '\r', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                            helper (List.drop in 5) nextOut
                        [x0, x1, x2, '\\', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                            helper (List.drop in 5) nextOut
                        [x0, x1, x2, '\\', '\r', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                            helper (List.drop in 6) nextOut
                        [x0, x1, x2, x3, '\\', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                                |> List.append x3
                            helper (List.drop in 6) nextOut
                        [x0, x1, x2, x3, '\\', '\r', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                                |> List.append x3
                            helper (List.drop in 7) nextOut
                        [x0, x1, x2, x3, x4, '\\', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                                |> List.append x3
                                |> List.append x4
                            helper (List.drop in 7) nextOut
                        [x0, x1, x2, x3, x4, '\\', '\r', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                                |> List.append x3
                                |> List.append x4
                            helper (List.drop in 8) nextOut
                        [x0, x1, x2, x3, x4, x5, '\\', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                                |> List.append x3
                                |> List.append x4
                                |> List.append x5
                            helper (List.drop in 8) nextOut
                        [x0, x1, x2, x3, x4, x5, '\\', '\r', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                                |> List.append x3
                                |> List.append x4
                                |> List.append x5
                            helper (List.drop in 9) nextOut
                        [x0, x1, x2, x3, x4, x5, x6, '\\', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                                |> List.append x3
                                |> List.append x4
                                |> List.append x5
                                |> List.append x6
                            helper (List.drop in 9) nextOut
                        [x0, x1, x2, x3, x4, x5, x6, '\\', '\r', '\n', ..] ->
                            nextOut =
                                out
                                |> List.append x0
                                |> List.append x1
                                |> List.append x2
                                |> List.append x3
                                |> List.append x4
                                |> List.append x5
                                |> List.append x6
                            helper (List.drop in 10) nextOut
                        _ ->
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

    removeBackSlashNewLinesRemaining remaining cleaned

removeBackSlashNewLinesRemaining = \in, out ->
    when in is
        ['\\', '\n', ..] ->
            removeBackSlashNewLinesRemaining (List.drop in 2) out
        ['\\', '\r', '\n', ..] ->
            removeBackSlashNewLinesRemaining (List.drop in 3) out
        [x, ..] ->
            nextOut = List.append out x
            removeBackSlashNewLinesRemaining (List.drop in 1) nextOut
        [] ->
            out

# Alternative impls to test performance

# This is just a simple reference impl for performance testing.
# removeBackSlashNewLines : List U8 -> List U8
# removeBackSlashNewLines = \src ->

#     removeBackSlashNewLinesRemaining src (List.withCapacity (List.len src))

# This version is testing the what the performance would be with explicit index.
# removeBackSlashNewLines : List U8 -> List U8
# removeBackSlashNewLines = \src ->
#     helper = \in, i, out ->
#         when List.get in i is
#             Ok '\\' ->
#                 when List.get in (i + 1) is
#                     Ok '\r' ->
#                         when List.get in (i + 2) is
#                             Ok '\n' ->
#                                 helper in (i + 3) out
#                             Ok x ->
#                                 nextOut =
#                                     out
#                                     |> List.append '\\'
#                                     |> List.append '\r'
#                                     |> List.append x
#                                 helper in (i + 3) nextOut
#                             Err _ ->
#                                 out
#                     Ok '\n' ->
#                         helper in (i + 2) out
#                     Ok x ->
#                         nextOut =
#                             out
#                             |> List.append '\\'
#                             |> List.append x
#                         helper in (i + 2) nextOut
#                     Err _ ->
#                         out
#             Ok x ->
#                 nextOut = List.append out x
#                 helper in (i + 1) nextOut
#             Err _ ->
#                 out

#     helper src 0 (List.withCapacity (List.len src))



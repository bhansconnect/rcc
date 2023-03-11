app "rcc"
    packages { pf: "../platform/main.roc" }
    imports [pf.Task.{ Task, await }, pf.Stderr, pf.File]
    provides [main] to pf

main : Str -> Task {} []
main = \filename ->
    bytes <- File.readBytes filename |> await
    when Str.fromUtf8 bytes is
        Ok str ->
            Stderr.raw str
        Err _ ->
            crash "so sad"

interface Stderr
    exposes [line, raw]
    imports [pf.Effect, Task.{ Task }]

line : Str -> Task {} *
line = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})

raw : List U8 -> Task {} *
raw = \bytes -> Effect.map (Effect.putRaw bytes) (\_ -> Ok {})

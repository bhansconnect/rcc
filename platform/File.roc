interface File
    exposes [readBytes]
    imports [pf.Effect, Task.{ Task }]

readBytes : Str -> Task.Task (List U8) *
readBytes = \name -> Effect.after (Effect.getFileBytes name) Task.succeed

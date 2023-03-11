hosted Effect
    exposes [Effect, after, map, always, forever, loop, getFileBytes, putLine, putRaw]
    imports []
    generates Effect with [after, map, always, forever, loop]

getFileBytes : Str -> Effect (List U8)

putLine : Str -> Effect {}

putRaw : Str -> Effect {}


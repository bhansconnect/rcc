# RCC: The Roc C Compiler

This is just a fun little test bed for trying to push the performance of the Roc programming language.
This will probably never be a functioning compiler. It may almost never be worked on.
That said, I hope to get some decent pieces complete to do some performance tests.

The theoretical end goal would be to create a ISO C compilant compiler (first C99 then later specs) that is competitive (hopefully faster than) [tcc](https://www.bellard.org/tcc/).
It of course would be only focused on dev builds. It would maybe do some minor optimization or half decent code gen.
One day, who knows it might even contain the linker.

Anyway, for now, the goal is to make a basic preprocessor.
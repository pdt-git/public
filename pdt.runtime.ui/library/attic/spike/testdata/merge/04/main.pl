:- use_module(foo).
:- use_module(fonk). %conflict, the program owning fonk also owns an extension of a module conflicting with foo.
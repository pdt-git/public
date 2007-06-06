:- use_module(foo).

/*
the program owning foo2 also owns an extension of foo.
in loading foo2, we need to rebind foo to this extension.
the original foo should remain defined - we don't own it.
*/
:- use_module(foo2). 
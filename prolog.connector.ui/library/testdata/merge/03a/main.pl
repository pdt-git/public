:- module(main,[]).


/*
the program owning foo also owns an extension of main.
in loading foo, we need merge its contributions into main.
*/
:- use_module(foo). 
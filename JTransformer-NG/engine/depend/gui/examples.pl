% Author:
% Date: 22.11.2002

/************************ 2 ******************************/
ask_name(Name) :-
        new(D, dialog('Register')),
        send(D, append,
             new(NameItem, text_item(name))),
        send(D, append,
             button(ok, message(D, return,
                                NameItem?selection))),
        send(D, append,
             button(cancel, message(D, return, @nil))),
        send(D, default_button, ok),
        get(D, confirm, Rval),
        free(D),
        Rval \== @nil,
        Name = Rval.

%?- ask_name(Name).

/************************ 3 ******************************/

:- use_module(library(pce)).

:- pce_begin_class(name_asker, dialog,
                   "Modal prompter for a name").

initialise(W, Label:[name]) :->
        "Initialise the window and fill it"::
        send(W, send_super, initialise, Label),
        send(W, append, text_item(name)),
        send(W, append, button(ok)),
        send(W, append, button(cancel)),
        send(W, default_button, ok).

ok(W) :->
        "User pressed the OK button"::
        get(W, member, name, NameItem),
        get(NameItem, selection, Typed),
        send(W, return, Typed).

cancel(W) :->
        "User pressed the Cancel button"::
        send(W, return, @nil).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value),
        free(W).

:- pce_end_class.

% get(name_asker('Register'), prompt, Name).




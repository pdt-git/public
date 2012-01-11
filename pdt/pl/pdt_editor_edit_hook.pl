
:- module(pdt_editor_edit_hook,[
]).


:- multifile(prolog_edit:edit_source/1).

prolog_edit:edit_source(Location) :-
    member(file(File), Location),
    member(line(Line), Location),
    format(atom(A), '~w ~w', [File, Line]),
    catch(pif_observe:pif_notify(pdt_edit_hook,A),_,true).

% Author: Daniel Speicher, Tobias Windeln
% Date: 25.10.2003

% Hilfsprädikate. Vorwiegend
% Prädikate aus den Modulen von Tobias und Uwe, die bei der
% Ausarbeitung des Beispiels Extract Interface verwendet wurden.


% same_member(_list1, _list2, _Member1, _Member2)
% ------------------------------------------------
% Gibt nacheinander alle Element zweier Listen zurück, die an gleicher Stelle
% stehen. Das Prädikat schlägt fehl für leere Listen.

% TODO: Wenn _list1 n und _list2 n+k Elemente hat, dann werden nur die ersten
% n-1 Elemente zurückgegeben. Es solten aber n sein. Allenfalls könnte man
% vertreten, dass in diesem Fall gar nichts zurückgeliefert wird.
%same_member([_h1], [_h2], _h1, _h2).
%same_member([_h1 | [_|_]], [_h2 | [_|_]], _h1, _h2).
%same_member([_| _t1],[_| _t2], _n1, _n2) :- same_member(_t1, _t2, _n1, _n2).

% Folgendes sollte statt obigem reichen -- Guenter:

same_member(_l1, _l2, _e1, _e2) :- same_position(_l1, _l2, _e1, _e2).

same_position([_h1 | _], [_h2 | _], _h1, _h2).
same_position([_| _t1],[_| _t2], _n1, _n2) :- same_member(_t1, _t2, _n1, _n2).



% =================================================================
%  Umsetzung von Abschnitt 2.1
%  All(P_1, List) von dort entspricht
%  declarationElement(_All, <Id der Klassendefinition von 'List'>)
% =================================================================


% declarationElement(_decl, _class)
% ---------------------------------
% _decl     Identifier of the fact representing the declartion element
% _class    The class to search for

% Local variables, parameters, fields
declarationElement(_decl, _class) :-
    varDefT(_decl, _, _, type(class, _class, _), _, _).

% Results of methods
declarationElement(_decl, _class) :-
    methodDefT(_decl, _, _, _, type(class, _class, _), _, _).


% declarationElementName(_decl, _Name)
% ------------------------------------
% _decl     Identifier of the fact representing the declartion element
% _Name     A name for the declaration element

% For variables we take the name
declarationElementName(_decl, _Name) :-
    varDefT(_decl, _, _, _, _Name, _).

% For methodresults we take the name of the method.
declarationElementName(_decl, _Name) :-
    methodDefT(_decl, _class, _methodName, _, _, _, _),
    classDefT(_class, _, _className, _),
    stringAppend(_className, '.', _methodName, _Name).

% We allow classes.
declarationElementName(_decl, _Name) :-
    classDefT(_decl, _, _Name, _).



% ===================================================================
%  Umsetzung von Definition 5 auf Seite 9.
%  Ergänzend wird hier die Ableitung für jede Deklaration ermittelt.
% ===================================================================


% declarationNonUpdateable(_decl, _class, _interface, _rule, _reasoning)
% ----------------------------------------------------------------------

declarationNonUpdateable(_decl, _class, _interface, 'A', _reasoning) :-
    declarationElement(_decl, _class),
    tcleqsome(_decl, _roots, _reason, _rule),
    not((member(_root, _roots), subtype(_interface, _root))),
    _reasoning = [('A', _reason, _rule)].

declarationNonUpdateable(_decl, _class, _interface, 'B', _reasoning) :-
    declarationElement(_decl, _class),
    tcleq(_decl, _superdecl, _reason, _rule),
    not(declarationElement(_superdecl, _class)),
    not(subtype(_interface, _superdecl)),
    _reasoning = [('B', _reason, _rule)].

declarationNonUpdateable(_decl, _class, _interface, 'C', _reasoning) :-
    declarationElement(_decl, _class),
    (
        tceq(_decl, _superdecl, _reason, _rule);
        tcleq(_decl, _superdecl, _reason, _rule);
        tcless(_decl, _superdecl, _reason, _rule)
    ),
    declarationNonUpdateable(_superdecl, _class, _interface, _, _subreasoning),
    append(_subreasoning, [('C', _reason, _rule)], _reasoning).
    
declarationNonUpdateable(_decl, _class, _interface) :-
    declarationNonUpdateable(_decl, _class, _interface, _, _).
    
    
    
%% effizientere Implementation von 'A' (von Tobias Windeln)
%% --------------------------------------------------------

%% Parameter
%% _decl   : Deklaration (Rückgabewert, lokale Variable, Parameter, Felder)
%% _class  : Klasse, die partiell durch das ...
%% _interf : ...Interface ersetzt werden soll.
%% _???    : Grund dafür, dass die Klasse verwendet werden muß.


badAccess(_decl, _class, _interf, _expr) :-
    applyT(_, _, _, _target, _),
    selectT(_target, _, _, _, _expr, _method),
    enclClass(_method, _class),
    identSelect(_expr, _, _, _, _, _decl),
    not(interfaceImplementsRootType(_method,_interf)).

interfaceImplementsRootType(_method,_interf) :-
    rootDefs(_method,_interfMethod),
    enclClass(_interfMethod, _superInterf),
    subtype(_interf,_superInterf).




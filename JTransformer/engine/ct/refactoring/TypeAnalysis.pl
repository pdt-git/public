% Author: Daniel Speicher
% Date: 31.10.2003

% Kleine Änderungen an realsubtype-Prädikat von GK, 4.11.2003

% Hilfsfunktionen zur Analyse von Typ, Interface und Klassendefinitionen

% Felder, Parameter, Variablen
typeDeclaration(_expr, _declel) :-
    identT(_expr, _, _, _, _declel).

typeDeclaration(_expr, _declel) :-
    selectT(_expr, _, _, _, _, _declel).

% Rückgabewerte
typeDeclaration(_expr, _declel) :-
  applyT(_expr, _, _, _target, _),
  selectT(_target, _, _, _, _, _declel).

% Konstruktoren
typeDeclaration(_expr, _declel) :-
  newClassT(_expr, _, _, _, _, _target, _, _),
  identT(_target, _, _, _, _declel).


% realsubtype(_sub, _super)
% -------------------------
% _sub:    Klasse oder Interface, die von ...
% _super:  ... Klasse oder Interface erbt.

realsubtype(_sub, _super) :-
    extendsT(_sub,_super).
realsubtype(_sub, _super) :-
    implementsT(_sub,_super).
realsubtype(_sub, _super) :-
    extendsT(_sub,_subsuper),
    realsubtype(_subsuper, _super).
realsubtype(_sub, _super) :-
    implementsT(_sub,_subsuper),
    realsubtype(_subsuper, _super).

subtype(_sub, _super) :-
    _sub = _super ; realsubtype(_sub, _super).

% TODO: Wenn _sub auf mehreren Wegen ein Untertyp von _super ist,
% erhalten wir das Paar mehrmals. Wollen wir das? Wenn nein, wie vermeiden
% wir das?

% Günter: Zwei Lösungen:
%  - Mit Seiteneffekten: Mittels assert die besuchten Elemente merken und
%    beim nächsten mal überspringen
%  - Ohne Seiteneffekte: mittels setof eine duplikatfreie Liste von
%    Ergebnissen generieren und danach diese schrittweise ablaufen und
%    zurückgeben.
% Die zweite Lösung ist sauberer, da man nicht das Problem hat, die
% globalen Seiteneffekte durch assert wieder rückgängig zu machen.
%
% Nachfolgend hab ich beides implementiert.
%
% 1. Hier die Variante mit Seiteneffekten nur als Beispiel wie man
% so etwas programmieren koennte. Was noch fehlt ist das
% Aufräumen der Prolog-DB / Beseitigung der Seiteneffekte.

realsubtypeWithSideeffects(_sub, _super) :-
    realsubtype(_sub, _super),
    firstResult(_sub, _super).

% Fails if there is already a remembered result.
% Otherwise it succeeds and remembers the result.
firstResult(_sub, _super) :-
   clause(rememberedRealsubtype(_sub,_super)),
   !,
   fail.
firstResult(_sub, _super) :-
   assert(rememberedRealsubtype(_sub,_super)).

forgetRealsubtype(_sub,_super) :-
    retract(rememberedRealsubtype(_sub,_super)).

% 2. Hier die Variante ohne Seiteneffekte. Evtl. hab ich die Reihenfolge
% der Parameter von setof vertauscht. Das koennt ihr aber in der Doku
% nachlesen.

realsubtypeWithoutSideeffects(_subtype, _supertype) :-
    setof(realsubtype(_sub, _super), realsubtype(_sub, _super), _resultSet),
    enumerate(_resultSet, _subtype, _supertype).

% At each backtracking step, arg2 and arg3 contains the arguments of
% another element of the list passed as arg1.
% Works only for elements unifiable with the term "realsubtype(_,_)".
% Other elements are ignored.
%
enumerate([realsubtype(_sub, _super)|_], _sub, _super).
enumerate([_|_rest], _sub, _super) :-
    enumerate(_rest,_sub,_super).

% parameterTypesEqual(_list1, _list2)
% -----------------------------------
% _list1:   Erste Parameterliste
% _list2:   Zweite Parameterliste
% Prüft ob die einander entsprechende Parameter in den Parameterlisten
% den gleichen Typ haben.

parameterTypesEqual([],[]).
parameterTypesEqual([_h1|_t1],[_h2|_t2]) :-
    varDefT(_h1, _, _, _type, _,_),
    varDefT(_h2, _, _, _type, _,_),
    parameterTypesEqual(_t1,_t2).


% overrides(_method1, _method2)
% -----------------------------
% _method1:   A method, which overrides ...
% _method2:   ... another method.

overrides(_method1, _method2) :-
    methodDefT(_method1, _sub, _name, _params1, _type, _, _),
    realsubtype(_sub, _super),
    methodDefT(_method2, _super, _name, _params2, _type, _, _),
    _method1 \= _method2,
    parameterTypesEqual(_params1, _params2).


% rootDefinition(_method, _methodroot)
% ------------------------------------
% _method     :  A method definition, which overrides the definition of ...
% _methodroot :  ... a root method, i.e. a method that doesn't override.
% _method may be itself the  root method.

rootDefinition(_method, _methodroot) :-
       (
        overrides(_method, _methodroot) ;
        _methodroot = _method
       ),
       not(overrides(_methodroot, _)).


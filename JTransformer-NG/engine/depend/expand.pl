
/******************************************************************************
 * Schritt 3) von depend/4 in depend.pl:
 *    expandAbstractionsAndDNF(+_c1, +_c2, +_t2, ?_c1dnf, ?_c2dnf, ?_t2dnf)
 ******************************************************************************
 * Expandieren von Abstraktionen zu DNF Termen. Do not backtrack
 *****************************************************************************/


:- multifile abstraction/1.
:- multifile spezialisation/1.
:- multifile tree/1.
:- multifile tree_id/2.
:- multifile test/1.


% Currently dead code:
% CT kann (muss nicht) in DNF sein.
expandCT(_ct, _Cexp, _Texp) :-
    ct(_ct, _c, _t),
    expandConditions(_c, _Cexp),
    expandActions(_t, _Texp).
    
/**
 * Arg3 arg4 und arg5 sind die zu DNF expandierten Versionen von arg1,2,3.
 */
expandAbstractionsAndDNF(_c1, _c2, _t2, _c1dnf, _c2dnf, _t2dnf) :-
    % 4.1) Filtern aller nicht-Programmelemente und Expandieren der Abstraktionen
    expandConditions(_c1, _c1e),
    %dnf(_c1e, _c1dnf),
    _c1dnf = _c1e,
    % 4.2) Filtern aller nicht-Programmelemente und Expandieren der Abstraktionen
    expandConditions(_c2, _c2e),
    dnf(_c2e, _c2dnf),
    % 4.3) _t2e ist automatisch in dnf, da _t2 eine reine Konjunktion sein muss
    expandActions(_t2, _t2dnf),
    !.

% todo: formt eine Bedingung in DNF um
%dnf(_c,_c).

% Specialisations are special abstractions,
% that have the same id as a contained element
% and cannot exist without this element:
abstraction(_x) :- spezialisation(_x).


/**
  Grundidee: Folge von Regeln, die eine Abstraktion beschreiben, wird umgeformt
  in einen logischen Ausdruck mit Konjunktion und Disjunktion.
  Alles was nicht Teil des Alphabets (tree/1) und nicht als Abstraktion
  markiert ist (abstraktion/1) wird ignoriert.
 */
expandConditions(_member, (_memberExp)) :-
    _member \= ','(_,_),
    _member \= ';'(_,_),
    expandCondition(_member, _memberExp).
expandConditions(','(_member,_t), ','(_memberExp,_T)) :-
    expandCondition(_member, _memberExp),
    expandConditions(_t, _T).
expandConditions(';'(_member,_t), (';'(_memberExp,_T))) :-
    expandCondition(_member, _memberExp),
    expandConditions(_t, _T).

expandCondition(_konjunction, (_Disjunction)) :-
    bagofT(_kexp, expandCondition_(_konjunction, _kexp), _l),
    semicolon2list(_Disjunction, _l).

expandCondition_(_abstraction, _abstraction) :-
    tree(_abstraction).
expandCondition_(not(_abstraction), not(_T)) :-
    not(tree(_abstraction)),
    abstraction(_abstraction),
    expandCondition_(_abstraction, _T).
expandCondition_(_abstraction, _T) :-
    not(tree(_abstraction)),
    abstraction(_abstraction),
    _abstraction \= not(_),
    % wenn es eine regel für diese abstraktion gibt
    clause(_abstraction, _body),
    % ersetze die abstraktion durch ihren expandierten body
    expandConditions(_body, _T).
expandCondition_(_abstraction, true) :- % lösche elemente die weder abstraktion noch tree sind
    not(tree(_abstraction)),
    not(abstraction(_abstraction)),
    _abstraction \= not(_).

expandActions(_konjunction, _Konjunction) :-
    bagofT(_kexp, expandActions_(_konjunction, _kexp), _l),
    semicolon2list(_Konjunction, _l). % be carefull with multiple solutions

expandActions_(_member, _memberExp) :-
    _member \= ','(_,_),
    expandAction_(_member, _memberExp).
expandActions_(','(_member,_t), (_memberExp,_T)) :-
    expandAction_(_member, _memberExp),
    expandActions_(_t, _T).

expandAction_(_abstraction, _abstraction) :-
    tree_action(_abstraction).
expandAction_(_abstraction, _T) :-
    not(tree_action(_abstraction)),
    abstract_action(_abstraction),
    clause(_abstraction, _body),
    expandActions_(_body, _T).
expandAction_(_abstraction, empty) :-
    not(tree_action(_abstraction)),
    abstract_action(_abstraction),
    not(clause(_abstraction, _body)).
expandAction_(_abstraction, empty) :-
    not(tree_action(_abstraction)),
    not(abstract_action(_abstraction)).

abstract_action(add(_)).
abstract_action(delete(_)).
abstract_action(replace(_,_)).
abstract_action(empty).

tree_action(add(_tree))             :- tree(_tree).
tree_action(delete(_tree))          :- tree(_tree).
tree_action(replace(_tree1,_tree2)) :- tree_action(delete(_tree1)),
                                       tree_action(add(_tree2)).
tree_action(empty).


/*************************************************************
 * Einige Tests
 */

   /*
abstraction(s).
s :- b, not(u).
s :- a, c.
abstraction(c).
c :- d.
abstraction(u).
u :- b, d.

tree(a).
tree(b).
tree(c).
tree_id(a,a).
tree_id(b,b).
tree_id(c,c).

add(s) :- add(c), add(u).



test('exp#2') :- expandConditions( (b;s),(b;( (b, not((b, true)));(a, c)))).
test('exp#3') :- expandConditions( (a;u),(a;(b, true))).
%test('exp#1') :- expandCT(expand1, (a, (not((b, not((b, true))));not((a, c)))), (add(c), empty)).
     */


% Author: Daniel Speicher, Tobias Windeln
% Date: 25.10.2003

:- ['TypeAnalysis.pl'].

% ======================================================================
%  Umsetzung der Tabelle Fig. 2 auf Seite 6.
%  Folgende Regeln sind implementiert:
%  (01), (03), (04), (06) - (09), (12), (14), (15)
%  Folgende Regeln sind implizit enthalten (durch die Definition von
%  typeDeclaration und die in den PEF enthaltenen annotations):
%  (02), (05), (13), (19), (20)
%  Noch nicht implementiert sind:
%  (10), (11), (16) - (18) (Echte Untertypen, Casts, Triviales)
% ======================================================================

%%% TODO: Gedanken darüber machen, welche Elemente man vergleichen kann.
%%% Erste Ergebnisse: Es scheint möglich zu sein, sich wirklich
%%% auf die Deklarationen (Rückgabewerte, Parameter, Variablen, Felder)
%%% zu beschränken. Weiter ist es sinnvoll, die Klassen (und Interface)
%%% noch hinzuzunehmen.



%% tceq: Die Element müssen den gleichen Typ haben
%% -----------------------------------------------

%% (02), (05) und (13), Typ der Rückgabe wie deklariert sind z. Zt.
%% implizit in typeDeclaration() enthalten.

%% (08), Überschriebe Methoden haben den gleichen Parametertypen
tceq(_declel1, _declel2, _reason, '(08)') :-
    overrides(_method1, _method2),
    _reason = _method1,
    methodDefT(_method2, _, _, _params1, _, _, _),
    methodDefT(_method1, _, _, _params2, _, _, _),
    member(_declel1, _params1),
    member(_declel2, _params2).

%% (09), Überschriebe Methoden haben den gleichen Rückgabetyp
%% Hier nur für Methoden, die Klassen zurückliefern ausgewertet.
tceq(_declel1, _declel2, _reason, '(09)') :-
    overrides(_declel1, _declel2),
    _reason = _declel1.


%% tcleq: Das erste Element muß vom zweiten abgeleitet sein.
%% ---------------------------------------------------------

%% (01) Bei Zuweisungen muss der Typ des zugewiesenen Wertes ein
%% Untertyp des Typs der Variable sein, zu der zugewiesen wird.
tcleq(_declel, _superdeclel, _reason, '(01)') :-
    assignT(_reason, _, _, _select, _value),
    typeDeclaration(_select, _superdeclel),
    typeDeclaration(_value,  _declel).

%% (01) Initialisierung einer lokalen Variablen. Der Typ des initialisierenden
%% Wertes muss vom Typ der Variablen abgeleitet sein.
tcleq(_declel, _superdeclel, _reason, '(01)') :-
    varDefT(_superdeclel, _, _, _, _, _reason),
    applyT(_reason, _, _, _select, _),
    typeDeclaration(_select, _declel).

%% (06) Bei Zugriff auf ein Feld, muss der Ausdruck auf dem (Dativ!) zugegriffen
%% wird von einer Klasse abgeleitet sein, die das Feld deklariert.
%% [Felddeklarationen unterscheiden sich von lokalen Variablen und Parametern
%% dadurch, dass sie in keiner Methode enthalten sind.]
tcleq(_declel, _superdeclel, _reason, '(06)') :-
    varDefT(_field, _superdeclel, _superdeclel, _, _, _),
    selectT(_reason, _, _, _, _on, _field),
    typeDeclaration(_on, _declel),
    _declel \= _superdeclel.   %% Der triviale Fall ist hier zu häufig ;-)

%% (03) und (14) Der Typ des aktuellen Parameters einer Methode 
%% muß vom deklarierten Parametertyp abgeleitet sein.
tcleq(_declel, _superdeclel, _reason, '(03/14)') :-
    applyT(_reason, _, _, _select, _args),
    selectT(_select, _, _, _, _, _method),
    methodDefT(_method,_ , _, _params, _, _, _),
    same_member(_args, _params, _arg, _superdeclel),
    typeDeclaration(_arg, _declel).

%% (07) Der Typ des zurückgegebenen Wertes muss vom deklarierten
%% Rückgabetyp abgeleitet sein.
tcleq(_declel, _superdeclel, _reason, '(07)') :-
    returnT(_reason, _, _superdeclel, _value),
    typeDeclaration(_value, _declel).

%% (12) Der Typ des aktuellen Parameters eines Konstruktors muß
%% vom deklarierten Parametertyp abgeleitet sein.
tcleq(_declel, _superdeclel, _reason, '(12)') :-
    newClassT(_reason, _, _, _constructor, _args, _, _, _),
    methodDefT(_constructor, _ , _, _params, _, _, _),
    same_member(_args, _params, _arg, _superdeclel),
    typeDeclaration(_arg, _declel).

%% tcleq ohne begründender Programmstelle und Regel
tcleq(_decel, _super) :-
    tcleq(_decel, _super, _, _).


%% tcless: Echte Untertypbeziehung. TODO.
%% --------------------------------------
tcless(_decel, _super, _reason, _rule) :-
    fail.
    
tcless(_decel, _super) :-
    tcless(_decel, _super, _, _).


%% tcleqsome: Das erste Element muß von einem Element der Liste abgeleitet sein
%% ----------------------------------------------------------------------------

tcleqsome(_declel, _roots, _reason, '(04/15)') :-
  applyT(_reason, _, _, _target, _),
  selectT(_target, _, _, _, _on, _method),
  typeDeclaration(_on, _declel),
  findall(_root, (rootDefinition(_method, _rootmethod),
     methodDefT(_rootmethod, _root, _, _, _, _, _)), _roots).

tcleqsome(_declel, _roots) :-
    tcleqsome(_declel, _roots, _, _).



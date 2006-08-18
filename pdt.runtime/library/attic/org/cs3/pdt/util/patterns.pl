%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
% 
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/**
 * pattern(_pattStr, _varList, _valStr).
 */
test('pattern/3#1') :- pattern('* bardey',[uwe],'uwe bardey').
test('pattern/3#2') :- pattern('uwe *',[bardey],'uwe bardey').
test('pattern/3#3') :- pattern('*',['uwe bardey'],'uwe bardey').
test('pattern/3#4') :- pattern('uwe * bardey',[tarek],'uwe tarek bardey').
test('pattern/3#5') :- pattern('* tarek *',[uwe,bardey],'uwe tarek bardey').
test('pattern/3#6') :- pattern('*tarek*',['uwe ',' bardey'],'uwe tarek bardey').
%test('pattern/3#7') :- not(pattern('* tarek *',_,_)).
%test('pattern/3#8') :- not(pattern('* tarek *',[uwe,_],_)).
test('pattern/3#9') :- not(pattern('* tarek *',_,'uwe bardey')).
test('pattern/3#10'):- not(pattern('*tarek*',[uwe,bardey],'uwe tarek bardey')).
test('pattern/3#11'):- not(pattern('aaa*',[_],'bbbb')).


/*
	pattern(+Pattern, ?WildcardReplacements, +Atom)

	Example:
	pattern('* tarek *',[uwe,bardey],'uwe tarek bardey')
	*/

pattern(_pattStr, _L, _valStr) :-
    ground(_valStr),
    !,
    % throw away first pattern if empty & move _valStr for _len positions
    nextPattern(_pattStr, _firstPatt, _restPatt),
    atom_prefix(_valStr, _firstPatt),
    atom_length(_firstPatt, _len),
    atom_length(_valStr, _vlen),
    plus(_diff, _len, _vlen),
    sub_atom(_valStr, _len, _diff, _, _restVal),
    patternRest(_restPatt, _L, _restVal).

pattern(_pattStr, _varList, _ValStr) :-
    ground(_varList),
    !,
    atom_chars(_pattStr, _pattList),
    replaceWildCards(_pattList, _varList, _valList),
    flatten( _valList, _ValList),
    %format('out: ~a',_ValList),
    atom_chars(_ValStr, _ValList).


% if pattern is empty and there's no string left, finish with empty var
patternRest('',[],'') :- !.
% if pattern is empty and there's a string left, put string in var list and finish
patternRest('',[_r],_r) :- !.
patternRest(_pattStr, [_h|_t], _valStr) :-
    nextPattern(_pattStr, _nextPatt, _restPatt),
    !,
    nextVarValue(_valStr, _nextPatt, _h, _restVal),
    patternRest(_restPatt, _t, _restVal).

nextPattern('', _, _) :- !, fail.
nextPattern(_pattStr, _NextPatt, _Rest) :-
    atom_chars(_pattStr, _l),
    nextSublist(_l, _L, _R),
    !,
    atom_chars(_NextPatt, _L),
    atom_chars(_Rest, _R).

/**
 * liefere die teilliste, bis zum nächsten * oder ?
 * nextSublist(_patternList, _NextPattern, _PatternRest).
 */
nextSublist([],[], []) :- !.
nextSublist(['*'|_t],[], _t) :- !.
%nextSublist(['?'|_t],[], _t) :- !. %TODO
nextSublist([_h|_t],[_h|_Y], _T) :- nextSublist(_t, _Y, _T ).

/**
 * nextVarValue(_str, _nextPatt, _Value, _restStr).
 */
nextVarValue(_str, _nextPatt, _Value, _restStr) :-
    % find next constant pattern
    sub_atom(_str, _pos, _len, _after, _nextPatt),
    !,
    % assign everything before this constant pattern to var value
    sub_atom(_str, 0   , _pos, _     , _Value),
    % compute rest string
    plus(_pos, _len, _newPos),
    sub_atom(_str, _newPos, _after, _, _restStr).

replaceWildCards([],[],[]) :- !.
replaceWildCards(['*'|_t],[_v|_vt],[_l|_T]) :-
    ground(_v),
    !,
    atom_chars(_v, _l),
    replaceWildCards(_t, _vt, _T).
replaceWildCards(['*'|_t],[_v|_vt],['*'|_T]) :-
    var(_v),
    !,
    %term_to_atom(_v, _vName),
    %atom_chars(_vName, _l),
    replaceWildCards(_t, _vt, _T).
replaceWildCards([_h|_t],_v,[_h|_T]) :-
    replaceWildCards(_t, _v, _T).

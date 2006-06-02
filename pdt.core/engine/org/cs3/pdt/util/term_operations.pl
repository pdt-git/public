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

comma_member(_member, _member) :- _member \= ','(_,_).
comma_member(_member, ','(_member,_)).
comma_member(_member, ','(_h,_t)) :-
    comma_member(_member, _t).

comma2list(_member, [_member]) :-
    _member \= ','(_,_),
    !.
comma2list(_member, [_member|_T]) :-
    nonvar(_T),
    _T = [],
    !.
comma2list(','(_member,_t), [_member|_T]) :-
    comma2list(_t, _T),
    !.
comma2list(_, []).

getTypeName(type(class, _typeid, _), _Typename) :-
    class(_typeid, _, _Typename),
    !.
getTypeName(type(basic, _Typename, _), _Typename) :-
    !.
getTypeName(_typeid, _Typename) :-
    class(_typeid, _, _Typename),
    !.

    
% reverse order of comma expression

comma_reverse(_c, _Crev) :-
    comma_reverse(_c, null, _Crev).
comma_reverse(','(_h, _t), null, _T) :-
    !,
    comma_reverse(_t, _h, _T).
comma_reverse(','(_h, _t), _c, _T) :-
    !,
    comma_reverse(_t, ','(_h,_c), _T).
comma_reverse(_h, null, _h) :- !.
comma_reverse(_h, _c, ','(_h,_c)) :- !.


%comma_map(_func, ','(_h, _t), ','(_fh, _t)) :-


% zweites argument sollte krzere liste sein -> bessere performanz
comma_append(_b, _a, _T) :-
    comma_reverse(_a, _ar),
    comma_prepend_(_ar, _b, _T),
    !.

comma_prepend_(','(_h, _t), _comma, _T) :-
    !,
    comma_prepend_(_t, ','(_h,(_comma)), _T).
comma_prepend_(_h, _comma, ','(_h,(_comma))) :- !.


comma_length(_c, _L) :-
    comma_length(_c, 1, _L).
comma_length(','(_h,_t), _l, _L) :-
    !,
    _l2 is _l + 1,
    comma_length(_t, _l2, _L).
comma_length(_h, _L, _L) :- !.

    

semicolon_member(_member, _member) :- _member \= ';'(_,_).
semicolon_member(_member, ';'(_member,_)).
semicolon_member(_member, ';'(_h,_t)) :- semicolon_member(_member, _t).

semicolon2list(_member, [_member]) :-
    _member \= ';'(_,_),
    !.
semicolon2list(_member, [_member|_T]) :-
    nonvar(_T),
    _T = [],
    !.
semicolon2list(';'(_member,_t), [_member|_T]) :-
    semicolon2list(_t, _T),
    !.
semicolon2list(_, []).

list_to_set_save(_l, _S) :-
    list_to_set_save(_l, [], _sr),
    reverse(_sr, _S).

list_to_set_save(_h,_S, _S) :-
    _h == [],
    !.
list_to_set_save([_h|_t], _s, _S) :-
    member_save(_h, _s),
    !,
    list_to_set_save(_t, _s, _S).
list_to_set_save([_h|_t], _s, _S) :-
    !,
    list_to_set_save(_t, [_h|_s], _S).

member_save(_m,[])      :- fail.
member_save(_m,[_h|_t]) :-
%     unify_non_shared(1, _m, _h),
     _m == _h.
member_save(_m,[_h|_t]) :-
%     unify_non_shared(1, _m, _h),
     member_save(_m, _t).


find_predicates(_namepattern,_list,_length) :-
   findall((_name,_num), (current_predicate(_name,_term), functor(_term,_,_num), pattern(_namepattern,_,_name)),_list),length(_list,_length).

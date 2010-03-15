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

/*

I have "borrowed" this code from a library contained in
the Yap Prolog system. Many thanks to the original author, Vitor Santos Costa, for making
his work available to the public.

The YAP Prolog implementation is actualy quiet interesting. Have a look:
http://www.ncc.up.pt/~vsc/Yap/

Changes i made to the original file:
- I have added is a set of predicates for ordered iteration of 
nodes starting from a given key value. 
- I have changed the predicate names to fit into
the pdt naming scheme.
- I have changed the file and module name.

The original file was released under the terms of the 

Perl Artistic License 
http://www.perl.com/pub/a/language/misc/Artistic.html


The original copyright notice follows below. 

Lukas Degener, 2006-03-31
*/


/*
update:
I changed a couple of other things, i do not quiet remember.

Thinks left to do:
  

 - refactor to use context terms
 - add status in nodes, most important: size, rank.
   this should cause no extra costs (only root --> change needs to be updated, we need to update this path anyway.)
 - add operations split and concat,
*/

/* 

	This code implements Red-Black trees as described in:

	"Introduction to Algorithms", Second Edition
	Cormen, Leiserson, Rivest, and Stein,
	MIT Press

        Author: Vitor Santos Costa

*/


:- module(pdt_util_rbtree,
	  [pdt_rbtree_new/1,
       pdt_rbtree_empty/1,
	   pdt_rbtree_lookup/3,
	   pdt_rbtree_lookupall/3,
	   pdt_rbtree_insert/4,
	   pdt_rbtree_delete/3,
	   pdt_rbtree_next/4,
	   pdt_rbtree_left/2,
	   pdt_rbtree_right/2
	   ]).

% create an empty tree.
pdt_rbtree_new(N):-
    node_nil(N).
pdt_rbtree_empty(N):-
    node_nil(N).



%context access
node_arity(5).
attribute_argnum(color,1).
attribute_argnum(left,2).
attribute_argnum(key,3).
attribute_argnum(value,4).
attribute_argnum(right,5).

node(N):-
	node_arity(A),
    functor(N,node,A).

red(N):-
	node(N),
    attribute(N,color,red).
    
black(N):-
	node(N),
    !,
    attribute(N,color,black).    
black(N):-
	nil(N).    

nil([]).
    

attribute(Node,_Attr,_Val):-
    nil(Node),
    !,
    fail.
attribute(Node,Attr,Val):-
    attribute_argnum(Attr,Num),
    arg(Num,Node,Val).

set_attribute(Nil,_Attr,_Val,_Out):-
    nil(Nil),
    !,
    fail.
set_attribute(In,Attr,Val,Out):-
	attribute_argnum(Attr,Num),
	node_arity(Arity),
	node(Out),
	set_attribute(In,Num,Val,Out,1,Arity).


set_attribute(_In,_Num,_Val,_Out,N,Arity):-
    N>Arity,
    !.
set_attribute(In,Num,Val,Out,Num,Arity):-    
    !,
    arg(Num,Out,Val),
    Next is Num + 1,
	set_attribute(In,Num,Val,Out,Next,Arity).
set_attribute(In,Num,Val,Out,N,Arity):-
    arg(N,In,Val),
    arg(N,Out,Val),
    Next is N + 1,
	set_attribute(In,Num,Val,Out,Next,Arity).
	

pdt_rbtree_left(Tree,Left):-
	attribute(Tree,left,Left).     


pdt_rbtree_right(Tree,Right):-
	attribute(Tree,right,Left).     
	
pdt_rbtree_new(K,V,Tree) :-
    black(Tree),
    attribute(Tree,left,Nil),
    attribute(Tree,right,Nil),
    attribute(Tree,key,K),
    attribute(Tree,value,V),
	nil(Nil).
	

	

%introduce an artificial lower bound
my_compare(<,'',_):-
	!.
my_compare(>,_,''):-
	!.
my_compare(=,'',''):-
	!.
my_compare(=,A,B):-
    unifiable(A,B,_),
    !.
my_compare(C,A,B):-
    A\=B,
    compare(C,A,B).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% searching.


%%
% pdt_rbtree_next(+At,-Key,-Val,+Tree)
%
% iterate all entries with keys equal to or greater than At.
%
% @param At the index value at which the search is to be started.
% @param Key is unified with the Key of each matching entry.
% @param Val is unified with the Value of each matching entry.
% @param The Red-Black-Tree to be searched.
pdt_rbtree_next(_, _, _, Tree) :- 
    nil(Tree),
	!, 
	fail.
pdt_rbtree_next(At,Key,Val,Tree):-
	attribute(Tree,key,KA),
	my_compare(Cmp,KA,At),
	lookup_next(Cmp,At,Key,Val,Tree).


    
lookup_next(<,At, K, V, Tree) :-
%    	writeln(left_off->look_right),
	attribute(Tree,right,NTree),
	pdt_rbtree_next(At,K, V, NTree).
lookup_next(>, At,K, V, Tree) :-
%    writeln(right_off->look_left),
	attribute(Tree,left,NTree),
	pdt_rbtree_next(At,K, V, NTree).
lookup_next(=, At,K, V, Tree) :-
%	writeln(match->look_left),
	attribute(Tree,left,NTree),
	pdt_rbtree_next(At,K, V, NTree).
lookup_next(=,_, K, V, Tree) :-
    attribute(Tree,key,K),
	attribute(Tree,value,V).
lookup_next(=, At,K, V, Tree) :-
%	writeln(match->look_right),
	attribute(Tree,right,NTree),
	pdt_rbtree_next(At,K, V, NTree).
lookup_next(>, _,K, V, Tree) :-
%    writeln(right_off->match),
    attribute(Tree,key,K),
	attribute(Tree,value,V).
lookup_next(>, At,K, V, Tree) :-
%    writeln(right_off->look_right),
	attribute(Tree,right,NTree),
	pdt_rbtree_next(At,K, V, NTree).


%pdt_rbtree_lookup(_, _, black([],_,_,[])) :- !, fail.

%% pdt_rbtree_lookup(+Key, -Val, +Tree) is det
% lookup the first matching entry in a R-B-Tree
% 
% @param Key the key too look up.
% @param Val will be unified with the value of the first matching entry.
% @param Tree the tree to be searched.
pdt_rbtree_lookup(_, _, Tree) :- 
    nil(Tree),
	!, 
	fail.
pdt_rbtree_lookup(Key, Val, Tree) :-
	attribute(Tree,key,KA),
	my_compare(Cmp,KA,Key),
	lookup(Cmp,Key,Val,Tree).

lookup(>, K, V, Tree) :-
	attribute(Tree,left,NTree),
	pdt_rbtree_lookup(K, V, NTree).
lookup(<, K, V, Tree) :-
	attribute(Tree,right,NTree),
	pdt_rbtree_lookup(K, V, NTree).
lookup(=, _, V, Tree) :-
	attribute(Tree,value,V).

%% pdt_rbtree_lookupall(+Key, -Val, +Tree)
% like lookup/3, but finds all matching entries.
pdt_rbtree_lookupall(Key, Val, T):-
    var(Key),
    !,
    pdt_rbtree_next('',Key,Val,T).
%pdt_rbtree_lookupall(_, _, black([],_,_,[])) :- !, fail.
pdt_rbtree_lookupall(_, _, Tree) :- 
	nil(Tree),
	!, 
	fail.
pdt_rbtree_lookupall(Key, Val, Tree) :-
	attribute(Tree,key,KA),
	my_compare(Cmp,KA,Key),
	lookupall(Cmp,Key,Val,Tree).

lookupall(>, K, V, Tree) :-
	attribute(Tree,left,NTree),
	pdt_rbtree_lookupall(K, V, NTree).
lookupall(=, _, V, Tree) :-
	attribute(Tree,value,V).
lookupall(=, K, V, Tree) :-
	attribute(Tree,left,NTree),
	pdt_rbtree_lookupall(K, V, NTree).
lookupall(=, K, V, Tree) :-
	attribute(Tree,right,NTree),
	pdt_rbtree_lookupall(K, V, NTree).
lookupall(<, K, V, Tree) :-
	attribute(Tree,right,NTree),
	pdt_rbtree_lookupall(K, V, NTree).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% manipulation.


%% pdt_rbtree_insert(+Tree0,+Key,+Val,-Tree) is det
% Tree insertion.
%
% Insert an entry in the tree. Note that this will _not_ 
% override any previous entry with the same key.
%
% @param Tree0 the original tree into which the entry is to be inserted.
% @param Key the key of the new entry.
% @param Val the value of the new entry.
% @param Tree will be unified with the tree resulting from the insertion.
%
pdt_rbtree_insert(Tree0,Key,Val,Tree) :-
    % We don't use parent nodes, so we may have to fix the root.
	insert2(Tree0,Key,Val,TreeI,_),
	fix_root(TreeI,Tree).

%
% make sure the root is always black.
%
/*fix_root(black(L,K,V,R),black(L,K,V,R)).
fix_root(red(L,K,V,R),black(L,K,V,R)).*/

fix_root(N,N):-
    black(N),
    !.
fix_root(Red,Black):-
    red(Red),
    set_attribute(Red,color,black,Black).

%
% Cormen et al present the algorithm as 
% (1) standard tree insertion;
% (2) from the viewpoint of the newly inserted node:
%     partially fix the tree;
%     move upwards
% until reaching the root.
%
% We do it a little bit different:
%
% (1) standard tree insertion;
% (2) move upwards:
%      when reaching a black node;
%        if the tree below may be broken, fix it.
% We take advantage of Prolog unification
% to do several operations in a single go.
%



%%
% insert2(+In, +K, +V, -Out, -Status) 
%
%insert2(black([],[],[],[]), K, V, T, Status) :- !,node(red
%insert2(+In, +K, +V, -Out, -Status) 
insert2(In, K, V, Out, Status) :- 
	nil(In),
	!,
	% empty tree / nil
	% replace it with a red node  that has two NIL children.
	%
	red(Out),
	attribute(Out,left,In),
	attribute(Out,right,In),
	attribute(Out,key,K),
	attribute(Out,value,V),
	Status = not_done.

%insert2(red(L,K0,V0,R), K, V, red(NL,K0,V0,R), Flag) :-
insert2(In, K, V, Out, Flag) :-
    % if the current node is red 
    % and the key beeing inserted is left of the current node.
    % recurse on the left subtree.
    node(red,In),
    attribute(In,key,K0),
	K @< K0, !,
	attribute(In,left,OldLeft),
	set_attribute(In,left,NewLeft,Out),	
	insert2(OldLeft, K, V, NewLeft, Flag).

%insert2(red(L,K0,V0,R), K, V, red(L,K0,V0,NR), Flag) :-
insert2(In, K, V, Out, Flag) :-
    % if the current node is red 
    % and the key beeing inserted is NOT left of the current node.
    % recurse on the right subtree.
    red(In),
    !,
    attribute(In,right,OldRight),
    set_attribute(In,right,NewRight,Out),
	insert2(OldRight, K, V, NewRight, Flag).
%insert2(black(L,K0,V0,R), K, V, NT, Flag) :-
insert2(In, K, V, Out, Flag) :-
    % if the current node is black
    % and the key beeing inserted is  left of the current node.
    % insert in the left subtree.
    % repair (how?)
    node(black,In),
    attribute(In,key,K0),
	K @< K0, !,
    attribute(In,left,OldLeft),
	insert2(OldLeft, K, V, NewLeft, Flag0),
	set_attribute(In,left,NewLeft,Tmp),
	fix_left(Flag0, Tmp, Out, Flag).
insert2(In, K, V, Out, Flag) :-
    % if the current node is black
    % and the key beeing inserted is NOT left of the current node.
    % insert in the right subtree.
    % repair (how?)
    node(black,In),
    !,
    attribute(In,right,OldRight),
	insert2(OldRight, K, V, NewRight, Flag0),
	set_attribute(In,right,NewRight,Tmp),
	fix_right(Flag0, Tmp, Out, Flag).


%
% How to fix if we have inserted on the left
%
fix_left(done,T,T,done) :- 
	% the damage has been repaired by a nested call 
	% in the left subtree.
	!.
fix_left(not_done,Tmp,Final,Done) :-
	fix_left(Tmp,Final,Done).


%
% case 1 of RB: just need to change colors.
%

%fix_left(
%	black(
%		red(Al,AK,AV,red(Be,BK,BV,Ga)),
%		KC,VC,
%		red(De,KD,VD,Ep)
%	),
%	red(
%		black(Al,AK,AV,red(Be,BK,BV,Ga)),
%		KC,VC,
%		black(De,KD,VD,Ep)
%	),
%	not_done) :- !.
fix_left(Tmp,Final,not_done):-
    black(Tmp),
    attribute(Tmp,left,A),
	    red(A),
	    attribute(A,right,B),
	    	red(B),
	attribute(Tmp,right,D),
	red(D),
	!,
	set_attribute(Tmp,color,red,Tmp1),
	set_attribute(A,color,black,A1),
	set_attribute(Tmp1,left,A1, Tmp2),
	set_attribute(D,color,black,D1),
	set_attribute(Tmp2,right,D1,Final).
	
%fix_left(
%	black(
%		red(red(Al,KA,VA,Be),KB,VB,Ga),
%		KC,VC,
%		red(De,KD,VD,Ep)
%	),
%	red(
%		black(red(Al,KA,VA,Be),	KB,VB,Ga),
%		KC,VC,
%		black(De,KD,VD,Ep)
%	),
%	not_done) :- !.
fix_left(Tmp,Final,not_done):-
	black(Tmp),
    attribute(Tmp,left,B),
	    red(B),
	    attribute(B,left,A),
	    	red(A),
	attribute(Tmp,right,D),
	red(D),
	!,
	set_attribute(Tmp,color,red,Tmp1),
	set_attribute(B,color,black,B1),
	set_attribute(Tmp1,left,B1, Tmp2),
	set_attribute(D,color,black,D1),
	set_attribute(Tmp2,right,D1,Final).

%% HERE %%

%
% case 2 of RB: got a knee so need to do rotations
%
fix_left(black(red(Al,KA,VA,red(Be,KB,VB,Ga)),KC,VC,De),
	black(red(Al,KA,VA,Be),KB,VB,red(Ga,KC,VC,De)),
	done) :- !.
%
% case 3 of RB: got a line
%
fix_left(black(red(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,De),
	black(red(Al,KA,VA,Be),KB,VB,red(Ga,KC,VC,De)),
	done) :- !.
%
% case 4 of RB: nothig to do
%
fix_left(T,T,done).

%
% How to fix if we have inserted on the right
%
fix_right(done,T,T,done) :- !.
fix_right(not_done,Tmp,Final,Done) :-
	fix_right(Tmp,Final,Done).

%
% case 1 of RB: just need to change colors.
%
fix_right(black(red(Ep,KD,VD,De),KC,VC,red(red(Ga,KB,VB,Be),KA,VA,Al)),
	red(black(Ep,KD,VD,De),KC,VC,black(red(Ga,KB,VB,Be),KA,VA,Al)),
	not_done) :- !.
fix_right(black(red(Ep,KD,VD,De),KC,VC,red(Ga,Ka,Va,red(Be,KB,VB,Al))),
	red(black(Ep,KD,VD,De),KC,VC,black(Ga,Ka,Va,red(Be,KB,VB,Al))),
	not_done) :- !.
%
% case 2 of RB: got a knee so need to do rotations
%
fix_right(black(De,KC,VC,red(red(Ga,KB,VB,Be),KA,VA,Al)),
	black(red(De,KC,VC,Ga),KB,VB,red(Be,KA,VA,Al)),
	done) :- !.
%
% case 3 of RB: got a line
%
fix_right(black(De,KC,VC,red(Ga,KB,VB,red(Be,KA,VA,Al))),
	black(red(De,KC,VC,Ga),KB,VB,red(Be,KA,VA,Al)),
	done) :- !.
%
% case 4 of RB: nothing to do.
%
fix_right(T,T,done).

%
% simplified processor
%
%
pdt_rbtree_pretty_print(T) :-
	pretty_print(T,6).

pretty_print(black([],[],[],[]),_) :- !.
pretty_print(red(L,K,_,R),D) :-
	DN is D+6,
	pretty_print(L,DN),
	format("~t~w: ~*|~n",[r,K,D]),
	pretty_print(R,DN).
pretty_print(black(L,K,_,R),D) :-
	DN is D+6,
	pretty_print(L,DN),
	format("~t~w: ~*|~n",[b,K,D]),
	pretty_print(R,DN).


pdt_rbtree_delete(T, K, NT) :-
	delete(T, K, NT, _).

%
% I am afraid our representation is not as nice for delete
%
delete(red(L,K0,V0,R), K, NT, Flag) :-
	my_compare(<,K,K0),!, %K @< K0, !,
	delete(L, K, NL, Flag0),
	fixup_left(Flag0,red(NL,K0,V0,R),NT, Flag).
delete(red(L,K0,V0,R), K, NT, Flag) :-
	my_compare(>,K,K0),!, %	K @> K0, !,
	delete(R, K, NR, Flag0),
	fixup_right(Flag0,red(L,K0,V0,NR),NT, Flag).
delete(red(L,_,_,R), _, OUT, Flag) :-
%	K == K0,
	delete_red_node(L,R,OUT,Flag).
delete(black(L,K0,V0,R), K, NT, Flag) :-
	my_compare(<,K,K0),!, %	K @< K0, !,
	delete(L, K, NL, Flag0),
	fixup_left(Flag0,black(NL,K0,V0,R),NT, Flag).
delete(black(L,K0,V0,R), K, NT, Flag) :-
	my_compare(>,K,K0),!, %	K @> K0, !,
	delete(R, K, NR, Flag0),
	fixup_right(Flag0,black(L,K0,V0,NR),NT, Flag).
delete(black(L,_,_,R), _, OUT, Flag) :-
%	K == K0,
	delete_black_node(L,R,OUT,Flag).

delete_red_node(L,L,L,done) :- !.
delete_red_node(black([],[],[],[]),R,R,done) :- !.
delete_red_node(L,black([],[],[],[]),L,done) :- !.
delete_red_node(L,R,OUT,Done) :-
	delete_next(R,NK,NV,NR,Done0),
	fixup_right(Done0,red(L,NK,NV,NR),OUT,Done).


delete_black_node(L,L,L,not_done) :- !.
delete_black_node(black([],[],[],[]),red(L,K,V,R),black(L,K,V,R),done) :- !.
delete_black_node(black([],[],[],[]),R,R,not_done) :- !.
delete_black_node(red(L,K,V,R),black([],[],[],[]),black(L,K,V,R),done) :- !.
delete_black_node(L,black([],[],[],[]),L,not_done) :- !.
delete_black_node(L,R,OUT,Done) :-
	delete_next(R,NK,NV,NR,Done0),
	fixup_right(Done0,black(L,NK,NV,NR),OUT,Done).


delete_next(red(black([],[],[],[]),K,V,R),K,V,R,done) :- !.
delete_next(black(black([],[],[],[]),K,V,red(L1,K1,V1,R1)),
	K,V,black(L1,K1,V1,R1),done) :- !.
delete_next(black(black([],[],[],[]),K,V,R),K,V,R,not_done) :- !.
delete_next(red(L,K,V,R),K0,V0,OUT,Done) :-
	delete_next(L,K0,V0,NL,Done0),
	fixup_left(Done0,red(NL,K,V,R),OUT,Done).
delete_next(black(L,K,V,R),K0,V0,OUT,Done) :-
	delete_next(L,K0,V0,NL,Done0),
	fixup_left(Done0,black(NL,K,V,R),OUT,Done).


fixup_left(done,T,T,done).
fixup_left(not_done,T,NT,Done) :-
	fixup2(T,NT,Done).


%
% case 1: x moves down, so we have to try to fix it again.
% case 1 -> 2,3,4 -> done
%
fixup2(black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(T1,KD,VD,black(Ep,KE,VE,Fi)),done) :- !,
	fixup2(red(black(Al,KA,VA,Be),KB,VB,black(Ga,KC,VC,De)),
		T1,
                _).
%
% case 2: x moves up, change one to red
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,black(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),done) :- !.
fixup2(black(black(Al,KA,VA,Be),KB,VB,black(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),not_done) :- !.
%
% case 3: x stays put, shift left and do a 4
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,black(red(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	red(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,black(Ep,KE,VE,Fi))),
	done) :- !.
fixup2(black(black(Al,KA,VA,Be),KB,VB,black(red(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,black(Ep,KE,VE,Fi))),
	done) :- !.
%
% case 4: rotate left, get rid of red
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,black(C,KD,VD,red(Ep,KE,VE,Fi))),
	red(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,black(Ep,KE,VE,Fi)),
	done).
fixup2(black(black(Al,KA,VA,Be),KB,VB,black(C,KD,VD,red(Ep,KE,VE,Fi))),
	black(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,black(Ep,KE,VE,Fi)),
	done).


fixup_right(done,T,T,done).
fixup_right(not_done,T,NT,Done) :-
	fixup3(T,NT,Done).


%
% case 1: x moves down, so we have to try to fix it again.
% case 1 -> 2,3,4 -> done
%
fixup3(black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(black(Fi,KE,VE,Ep),KD,VD,T1),done) :- !,
        fixup3(red(black(De,KC,VC,Ga),KB,VB,black(Be,KA,VA,Al)),T1,_).

%
% case 2: x moves up, change one to red
%
fixup3(red(black(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	done) :- !.
fixup3(black(black(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	not_done):- !.
%
% case 3: x stays put, shift left and do a 4
%
fixup3(red(black(black(Fi,KE,VE,Ep),KD,VD,red(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	red(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,black(Ga,KB,VB,black(Be,KA,VA,Al))),
	done) :- !.
fixup3(black(black(black(Fi,KE,VE,Ep),KD,VD,red(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,black(Ga,KB,VB,black(Be,KA,VA,Al))),
	done) :- !.
%
% case 4: rotate right, get rid of red
%
fixup3(red(black(red(Fi,KE,VE,Ep),KD,VD,C),KB,VB,black(Be,KA,VA,Al)),
	red(black(Fi,KE,VE,Ep),KD,VD,black(C,KB,VB,black(Be,KA,VA,Al))),
	done).
fixup3(black(black(red(Fi,KE,VE,Ep),KD,VD,C),KB,VB,black(Be,KA,VA,Al)),
	black(black(Fi,KE,VE,Ep),KD,VD,black(C,KB,VB,black(Be,KA,VA,Al))),
	done).

%
% This code checks if a tree is ordered and a rbtree
%
%
rbtree(black([],[],[],[])) :- !.
rbtree(T) :-
	catch(rbtree1(T),msg(S,Args),format(S,Args)).

rbtree1(black(L,K,_,R)) :-
	find_path_blacks(L, 0, Bls),
	check_rbtree(L,-1000000,K,Bls),
	check_rbtree(R,K,1000000,Bls).
rbtree1(red(_,_,_,_)) :-
	throw(msg("root should be black",[])).
	

find_path_blacks(black([],[],[],[]), Bls, Bls) :- !.
find_path_blacks(black(L,_,_,_), Bls0, Bls) :-
	Bls1 is Bls0+1,
	find_path_blacks(L, Bls1, Bls).
find_path_blacks(red(L,_,_,_), Bls0, Bls) :-
	find_path_blacks(L, Bls0, Bls).

check_rbtree(black([],[],[],[]),Min,Max,Bls0) :- !,
	check_height(Bls0,Min,Max).
check_rbtree(red(L,K,_,R),Min,Max,Bls) :-
	check_val(K,Min,Max),
	check_red_child(L),
	check_red_child(R),
	check_rbtree(L,Min,K,Bls),
	check_rbtree(R,K,Max,Bls).
check_rbtree(black(L,K,_,R),Min,Max,Bls0) :-
	check_val(K,Min,Max),
	Bls is Bls0-1,
	check_rbtree(L,Min,K,Bls),
	check_rbtree(R,K,Max,Bls).

check_height(0,_,_) :- !.
check_height(Bls0,Min,Max) :-
	throw(msg("Unbalance ~d between ~w and ~w~n",[Bls0,Min,Max])).

check_val(K, Min, Max) :- K > Min, K < Max, !.
check_val(K, Min, Max) :- 
	throw(msg("not ordered: ~w not between ~w and ~w~n",[K,Min,Max])).

check_red_child(black(_,_,_,_)).
check_red_child(red(_,K,_,_)) :-
	throw(msg("must be red: ~w~n",[K])).


%count(1,16,X), format("deleting ~d~n",[X]), new(1,a,T0), insert(T0,2,b,T1), insert(T1,3,c,T2), insert(T2,4,c,T3), insert(T3,5,c,T4), insert(T4,6,c,T5), insert(T5,7,c,T6), insert(T6,8,c,T7), insert(T7,9,c,T8), insert(T8,10,c,T9),insert(T9,11,c,T10), insert(T10,12,c,T11),insert(T11,13,c,T12),insert(T12,14,c,T13),insert(T13,15,c,T14), insert(T14,16,c,T15),delete(T15,X,T16),pretty_print(T16),rbtree(T16),fail.

% count(1,16,X0), X is -X0, format("deleting ~d~n",[X]), new(-1,a,T0), insert(T0,-2,b,T1), insert(T1,-3,c,T2), insert(T2,-4,c,T3), insert(T3,-5,c,T4), insert(T4,-6,c,T5), insert(T5,-7,c,T6), insert(T6,-8,c,T7), insert(T7,-9,c,T8), insert(T8,-10,c,T9),insert(T9,-11,c,T10), insert(T10,-12,c,T11),insert(T11,-13,c,T12),insert(T12,-14,c,T13),insert(T13,-15,c,T14), insert(T14,-16,c,T15),delete(T15,X,T16),pretty_print(T16),rbtree(T16),fail.

count(I,_,I).
count(I,M,L) :-
	I < M, I1 is I+1, count(I1,M,L).

test_pos :-
	new(1,a,T0),
	N = 10000,
	build_ptree(2,N,T0,T),
%	pretty_print(T),
	rbtree(T),
	clean_tree(1,N,T,_),
	bclean_tree(N,1,T,_),
	count(1,N,X), ( delete(T,X,TF) -> true ; abort ),
%	pretty_print(TF), 
	rbtree(TF),
	format("done ~d~n",[X]),
	fail.
test_pos.

build_ptree(X,X,T0,TF) :- !,
	insert(T0,X,X,TF).
build_ptree(X1,X,T0,TF) :-
	insert(T0,X1,X1,TI),
	X2 is X1+1,
	build_ptree(X2,X,TI,TF).


clean_tree(X,X,T0,TF) :- !,
	delete(T0,X,TF),
	( rbtree(TF) -> true ; abort).
clean_tree(X1,X,T0,TF) :-
	delete(T0,X1,TI),
	X2 is X1+1,
	( rbtree(TI) -> true ; abort),
	clean_tree(X2,X,TI,TF).

bclean_tree(X,X,T0,TF) :- !,
	format("cleaning ~d~n", [X]),
	delete(T0,X,TF),
	( rbtree(TF) -> true ; abort).
bclean_tree(X1,X,T0,TF) :-
	format("cleaning ~d~n", [X1]),
	delete(T0,X1,TI),
	X2 is X1-1,
	( rbtree(TI) -> true ; abort),
	bclean_tree(X2,X,TI,TF).



test_neg :-
	Size = 10000,
	new(-1,a,T0),
	build_ntree(2,Size,T0,T),
%	pretty_print(T),
	rbtree(T),
	MSize is -Size,
	clean_tree(MSize,-1,T,_),
	bclean_tree(-1,MSize,T,_),
	count(1,Size,X), NX is -X, ( delete(T,NX,TF) -> true ; abort ),
%	pretty_print(TF), 
	rbtree(TF),
	format("done ~d~n",[X]),
	fail.
test_neg.

build_ntree(X,X,T0,TF) :- !,
	X1 is -X,
	insert(T0,X1,X1,TF).
build_ntree(X1,X,T0,TF) :-
	NX1 is -X1,
	insert(T0,NX1,NX1,TI),
	X2 is X1+1,
	build_ntree(X2,X,TI,TF).



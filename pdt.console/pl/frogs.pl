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
solves a common type of card puzzle where you have 
nine cards which have to be positioned into a NxM grid
so that the motives painted on each of the four card borders
are alligned with corresponding motives on the
adjacent cards.
*/

/*
specify grid dimensions
*/
grid(3,3).

/*
card/5 
the facts that represent the card deck

facts are of the form card(cardId, top,right,bottom,left)
(exact orientation does not matter, see oriented_card/5)
*/
card(0,tail(yellow),tail(green),head(yellow),tail(blue)).
card(1,tail(blue),head(red),head(green),head(red)).
card(2,head(blue),head(yellow),tail(red),head(green)).
card(3,head(blue),head(green),head(yellow),head(yellow)).
card(4,head(red),tail(green),tail(red),tail(blue)).
card(5,tail(green),tail(yellow),head(blue),head(green)).
card(6,tail(red),tail(yellow),tail(yellow),tail(green)).
card(7,tail(blue),head(blue),head(green),tail(red)).
card(8,head(red),tail(red),tail(blue),head(yellow)).

%card(9,head(blue),head(yellow),tail(red),head(green)).
%card(10,head(red),head(blue),head(green),head(blue)).
%card(11,tail(green),tail(red),tail(blue),tail(red)).


/*
succeeds if a card exist that fits the specified clockwise order of symbols.
*/
oriented_card(Id,A,B,C,D):-    
    card(Id,A,B,C,D).
oriented_card(Id,A,B,C,D):-
    card(Id,D,A,B,C).
oriented_card(Id,A,B,C,D):-    
    card(Id,C,D,A,B).
oriented_card(Id,A,B,C,D):-    
    card(Id,B,C,D,A).
    
    
/*
succeeds if argument is a lis of tupples of the form
tuple(Id,Top,Right,Bottom,Left), that satisfies all constraints 
imposed by the games rules. 
Order of the tupples should be inversed i.e., the top left card should be tha last 
in the list, the card lying right to the top left card should be the second last etc.
*/
word([]).
word([H|T]):-
	word(T),
	next(H,T).    

/*
produces a tuple that could be appended to the next position.
*/
next(tuple(Id,Top,Right,Bottom,Left),Done):-
    oriented_card(Id,Top,Right,Bottom,Left),
    \+ member(tuple(Id,_,_,_,_),Done),
    length(Done,Pos),
    fit_top(Top,Done,Pos),
    fit_left(Left,Done,Pos).
    
fit_top(_,_,Pos):-
    grid(I,_),
    Pos < I,
    !.
fit_top(Top,Done,_):-
    grid(I,_),
    nth1(I,Done,tuple(_,_,_,Peer,_)),
    frog_fits(Peer,Top).

fit_left(_,_,Pos):-
    grid(M,_),
    0 is Pos mod M,
    !.
fit_left(Left,[tuple(_,_,Peer,_,_)|_],_):-
    frog_fits(Peer,Left).

frog_fits(head(A),tail(A)). 
frog_fits(tail(A),head(A)).

prolog_trace_interception(Port,Frame,_,continue):-
    writeln(port(Port)),
    prolog_frame_attribute(Frame,goal,Goal),
    writeln(goal(Goal)),
    prolog_frame_attribute(Frame,pc,PC),
    writeln(pc(PC)),
    nl.

tester:-
    writeln(a),
        writeln(a),
            writeln(a).

/*
 prints out all solutions to the current game settings.
*/
solve:-
    solve(L),
    forall(member(A,L),writeln(A)),
    nl,
    fail.

solve(L):-
    grid(M,N),
	Len is M*N,
	length(L,Len),
	word(L).
	
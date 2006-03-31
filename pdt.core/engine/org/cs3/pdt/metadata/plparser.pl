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

:- module(plparser,[parse/1,parse/2,node_attr/2]). 

:- use_module(plast).
:- dynamic node_attr/2.
:- dynamic node_id/1.

          
parse(File,InStream):-
    writeln(ta),
    plast_new_node(compilation_unit,Id),
    plast_set_prop(Id,type(compilation_unit)),
    plast_set_prop(Id,file(File)),
   	parse_clauses(InStream,Id,user).


parse(File):-
    writeln(tu),
    open(File,read,InStream),
    parse(File,InStream),
    close(InStream).


parse_clauses(InStream,FileId,CurrentModule):-    
	parse_clause(InStream,FileId,CurrentModule,NextModule,ClauseId),
	(	ClauseId==end_of_file
	->	true
	;	plast_set_prop(FileId,member(ClauseId)),
		parse_clauses(InStream,FileId,NextModule)
	).

    

parse_clause(InStream,ParentId,CurrentModule,NextModule,ClauseId):-
	catch(
		read_term(InStream,Term,
			[	term_position(Pos),
				subterm_positions(Sub),
				module(CurrentModule),
				variable_names(VarNames),
				singletons(Singletons)
			]),
		error(Error,Context),
		(handle_error(ClauseId,Error,Context),fail)
	),	
    format("processing ~w~n",[Term]),
	arg(2,Pos,Line)	,
	(	Term==end_of_file
	->	ClauseId=end_of_file
	;   next_module(Term,NextModule),
		numbervars(VarNames,0,_),
		b_setval(bindings,VarNames),
		parse_subterm_positions(ParentId,Term,Sub,ClauseId),
	   	plast_set_prop(ClauseId,line(Line)),      
		plast_set_prop(ClauseId,toplevel_term),
		plast_set_prop(ClauseId,singletons(Singletons)),
		plast_set_prop(ClauseId,module(CurrentModule))
	).



next_module(Term,NextModule):-
    Term=..[:-,Directive],
    Directive=..[module,NextModule,_],
    !.
next_module(_,user).
    
handle_error(Id,Error,Context,File):-
    plast_new_node(error,Id),
	plast_set_prop(Id,error(Error)),    
	plast_set_prop(Id,context(Context)),
	plast_set_prop(Id,file(File)).
	
assert_common_attrs(Id,Type,Parent,From-To,Term):-
    plast_set_prop(Id,type(Type)),
    plast_set_prop(Id,parent(Parent)),
    plast_set_prop(Id,From-To),
    plast_set_prop(Id,term(Term)).
var_name(Var,Name):-
	b_getval(bindings,B),
	(	member(Name=C,B),C==Var   
	->	true
	;	C='_'
	).
	
parse_subterm_positions(ParentId,Term, From-To,Id):-	
	( ground(Term)
	->plast_new_node(atom,Id),
	  assert_common_attrs(Id,atom,ParentId,From-To,Term)
	; plast_new_node(variable,Id),
	  assert_common_attrs(Id,variable,ParentId,From-To,Term),
	  var_name(Term,Name),
	  plast_set_prop(Id,name(Name))
	).
	
parse_subterm_positions(ParentId,Term, string_position(From,To),Id):-
	plast_new_node(string,Id),
	assert_common_attrs(Id,string,ParentId,From-To,Term).

parse_subterm_positions(ParentId,Term,  brace_term_position(From, To, Arg),Id):-
    plast_new_node(brace,Id),
	assert_common_attrs(Id,brace,ParentId,From-To,Term),
	arg(1,Term,ArgTerm),
	parse_subterm_positions(Id,ArgTerm,Arg,ArgId),
	plast_set_prop(Id,argument(ArgId)).

parse_subterm_positions(ParentId,Term,  list_position(From, To, Elms, Tail),Id):-
   	plast_new_node(list,Id),
	assert_common_attrs(Id,list,ParentId,From-To,Term),
	parse_elm_positions(Id,Term,Elms,ElmIDs,TailTerm),
	plast_set_prop(Id,elements(ElmIDs)),
	(	Tail==none
	->	true
	;	parse_subterm_positions(Id,TailTerm,Tail,TailId),
		plast_set_prop(Id,tail(TailId))
	).
    
parse_subterm_positions(ParentId,Term,  term_position(From, To, FFrom, FTo, SubPos),Id):-
    plast_new_node(compound,Id),
    assert_common_attrs(Id,compound,ParentId,From-To,Term),
    plast_set_prop(Id,functor_position(FFrom-FTo)),
    functor(Term,Name,Arity),
    plast_set_prop(Id,functor(Name/Arity)),
    Term=..[_|Args],
   	parse_arg_positions(Id,Args,SubPos,ArgIDs),
   	plast_set_prop(Id,arguments(ArgIDs)).

parse_arg_positions(_,[],[],[]).   	

parse_arg_positions(ParentId,[Arg|Args],[Position|Positions],[Id|Ids]):-
   	parse_subterm_positions(ParentId,Arg,Position,Id),
   	parse_arg_positions(ParentId,Args,Positions,Ids).

%parse_elm_positions(+Parent,+Term,+ElmPositions,-ElmIds,-TailTerm)
parse_elm_positions(_,TailTerm,[],[],TailTerm).   	

parse_elm_positions(ParentId,[HeadTerm|RestTerm],[HeadPos|RestPos],[HeadId|RestIds],TailTerm):-
   	parse_subterm_positions(ParentId,HeadTerm,HeadPos,HeadId),
   	parse_elm_positions(ParentId,RestTerm,RestPos,RestIds,TailTerm).
   	

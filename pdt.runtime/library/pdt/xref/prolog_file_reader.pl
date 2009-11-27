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

:- module(plparser_neu,[parse/1,parse/2,node_attr/2]). 

:- dynamic node_attr/2.
:- dynamic node_id/1.

/**
 * parse(?File)
 * 	opens a stream to the file Arg1, tries to parse it as a prolog program and
 * 	closes the stream afterwards. 
 */
parse(File):-
    open(File,read,InStream),
    parse(File,InStream),
    close(InStream).
          
/**
 * parse(+File, +InStream)
 * 	creates some unic facts for the file Arg1 and starts the parsing of the
 * 	clauses contained in the stream Arg2 (which should be one to the file Arg1).
 */          
parse(File,InStream):-
    new_node_id(Id),	%Eva: I think the Lukas version the type of the node was part of the unique id.
    assert_unique_fact(node_id(Id)),
    assert_unique_fact(node_attr(Id,type(compilation_unit))),
    assert_unique_fact(node_attr(Id,file(File))),
    parse_clauses(InStream,Id,user).


/**
 * parse_clauses(+InStream,+FileId,+CurrentModule)
 * 	succeeds if parse_clause/5 succeeds.
 * 	starts a parse the clauses in the stream Arg1 and file Arg2
 * 	rekursively and assigns the clause to the file.
 * 	If one clause changes the module the recursion works on the new
 * 	module.
 * 	Stops if a clause is EoF. 
 */
parse_clauses(InStream,FileId,CurrentModule):-    
    parse_clause(InStream,FileId,CurrentModule,NextModule,ClauseId),
    (   ClauseId==end_of_file
    ->  true
    ;   assert_unique_fact(node_attr(FileId,member(ClauseId))),
        parse_clauses(InStream,FileId,NextModule)
    ).

    
/**
 * parse_clause(+InStream,+FileId,+CurrentModule,?NextModule,?ClauseId)
 *	reads the first term from Arg1 if it is not EoF it sets Arg4 to a new module,
 *  parses possible subterms recursively and asserts some unic facts about the term.
 */
parse_clause(InStream,FileId,CurrentModule,NextModule,ClauseId):-
    catch(
        read_term(InStream,Term,
            [   term_position(Pos),        % output
                subterm_positions(Sub),    % output
                module(CurrentModule),     % INput
                variable_names(VarNames),  % output
                singletons(Singletons)     % output
            ]),
        error(Error,Context),
        ( node_attr(FileId,file(File)),               % <<<<
          handle_error(ClauseId,Error,Context,File),  % <<<<
          fail
        )  
    ),  
   % format("processing ~w~n",[Term]),
    (   Term==end_of_file
    ->  ClauseId=end_of_file
    ;   next_module(Term,CurrentModule,NextModule),	% <<<<
        numbervars(VarNames,0,_),
        parse_subterm_positions(FileId,Term,Sub,VarNames,ClauseId),
        arg(2,Pos,Line) ,
        assert_unique_fact(node_attr(ClauseId,line(Line))),      
        assert_unique_fact(node_attr(ClauseId,toplevel_term)),
        assert_unique_fact(node_attr(ClauseId,singletons(Singletons))),
        assert_unique_fact(node_attr(ClauseId,module(CurrentModule)))
    ).

/**
 * next_module(+Term,?NextModule)
 * 	succeeds if Arg1 is a module definition of module Arg2
 * 	or Arg2 is user.
 */
next_module(Term,_,NextModule):-
    Term=..[:-,Directive],
    Directive=..[module,NextModule,_],
    !.
next_module(_,Current,Current).
    
/**
 * handle_error(+Id,+Error,+Context,+File)
 * adds an error node with id Arg1 that contains Arg2 - Arg4 as attributs.
 */    
handle_error(Id,Error,Context,File):-
    new_node_id(Id),	%Eva: I think the Lukas version the type of the node was part of the unique id.
    assert_unique_fact(node_id(Id)),
    assert_unique_fact(node_attr(Id,error(Error))),    
    assert_unique_fact(node_attr(Id,context(Context))),
    assert_unique_fact(node_attr(Id,file(File))).
/*   plast_new_node(error,Id),
    plast_set_prop(Id,error(Error)),    
    plast_set_prop(Id,context(Context)),
    plast_set_prop(Id,file(File)).
*/
        
        
        
parse_subterm_positions(FileId,Term, From-To,VarNames,Id):-    
    new_node_id(Id),
    assert_unique_fact(node_id(Id)),
    ( 	ground(Term)
    ->	
      	assert_common_attrs(Id,atom,FileId,From-To,Term)
    ; 	
      	assert_common_attrs(Id,variable,FileId,From-To,Term),
        var_name(Term,VarNames,Name),
      	assert_unique_fact(node_attr(Id,name(Name)))
    ).
    
parse_subterm_positions(FileId,Term, string_position(From,To),_,Id):-
    new_node_id(Id),
    assert_unique_fact(node_id(Id)),
    assert_common_attrs(Id,string,FileId,From-To,Term).

parse_subterm_positions(FileId,Term,  brace_term_position(From, To, Arg),VarNames,Id):-
    new_node_id(Id),
    assert_unique_fact(node_id(Id)),
    assert_common_attrs(Id,brace,FileId,From-To,Term),
    arg(1,Term,ArgTerm),
    parse_subterm_positions(Id,ArgTerm,Arg,VarNames,ArgId),
    assert_unique_fact(node_attr(Id,argument(ArgId))).

parse_subterm_positions(FileId,Term,  list_position(From, To, Elms, Tail),VarNames,Id):-
    new_node_id(Id),
    assert_unique_fact(node_id(Id)),
    assert_common_attrs(Id,list,FileId,From-To,Term),
    parse_elm_positions(Id,VarNames,Term,Elms,ElmIDs,TailTerm),
    assert_unique_fact(node_attr(Id,elements(ElmIDs))),
    (   Tail==none
    ->  true
    ;   parse_subterm_positions(Id,TailTerm,Tail,VarNames,TailId),
        assert_unique_fact(node_attr(Id,tail(TailId)))
    ).
    
parse_subterm_positions(FileId,Term,  term_position(From, To, FFrom, FTo, SubPos),VarNames,Id):-
    new_node_id(Id),
    assert_unique_fact(node_id(Id)),
    assert_common_attrs(Id,compound,FileId,From-To,Term),
    assert_unique_fact(node_attr(Id,functor_position(FFrom-FTo))),
    functor(Term,Name,Arity),
    assert_unique_fact(node_attr(Id,functor(Name/Arity))),
    Term=..[_|Args],
    parse_arg_positions(Id,VarNames,Args,SubPos,ArgIDs),
    assert_unique_fact(node_attr(Id,arguments(ArgIDs))).
    
    

parse_arg_positions(_,_,[],[],[]).    

parse_arg_positions(FileId,VarNames,[Arg|Args],[Position|Positions],[Id|Ids]):-
    parse_subterm_positions(FileId,Arg,Position,VarNames,Id),
    parse_arg_positions(FileId,VarNames,Args,Positions,Ids).

%parse_elm_positions(+Parent,VarNames,+Term,+ElmPositions,-ElmIds,-TailTerm)
parse_elm_positions(_,_,TailTerm,[],[],TailTerm).     

parse_elm_positions(FileId,VarNames,[HeadTerm|RestTerm],[HeadPos|RestPos],[HeadId|RestIds],TailTerm):-
    parse_subterm_positions(FileId,HeadTerm,HeadPos,VarNames,HeadId),
    parse_elm_positions(FileId,VarNames,RestTerm,RestPos,RestIds,TailTerm).
    
 
 
/**
 * assert_common_attrs(+Id,+Type,+Parent,+From-To,+Term)
 * 	asserts Arg2 - Arg4 as node attributes to the node Arg1 if 
 * 	they were not set before. 
 */    
assert_common_attrs(Id,Type,Parent,From-To,Term):-
    assert_unique_fact(node_attr(Id,type(Type))),
    assert_unique_fact(node_attr(Id,parent(Parent))),
    assert_unique_fact(node_attr(Id,From-To)),
    assert_unique_fact(node_attr(Id,term(Term))).
 
 
cleanup_node_attrs:-
	retractall(node_id(_)),
	retractall(node_attr(_,_)).


var_name(Var,Bindings,Name):-
    (   member(Name=C,Bindings),C==Var   
    ->  true
    ;   C='_'
    ).
  
	    
/*var_name(Var,Name):-
    b_getval(bindings,B),
    (   member(Name=C,B),C==Var   
    ->  true
    ;   C='_'
    ).
*/    
    
    
/*    
 
unused_id(Type,Id):-
    atom_concat(Type,'_node',Fun),
	Num is random(1073741824),
	Try=..[Fun,Num],
    ( node_id(Try)
    ->unused_id(Type,Id)
    ; Id=Try
    ).	

% plast_new_node(+Type,-Id)
% creates a new node of a given type
plast_new_node(Type,Id):-
    unused_id(Type,Id),
    my_assert(node_id(Id)).
    
    
 my_assert(node_id(Id)):-
    ( 	plast_node(Id)
    ->	true
	;   assert(node_id(Id))
	).

my_assert(node_attr(Id,Attr)):-
    (	plast_prop(Id,Attr)
    ->	true
    ;	assert(node_attr(Id,Attr))
    ).
*/    
    
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

:-module(builtin_predicate_factory,[
	pdt_index_builtins/0
	]).



:- use_module(library('org/cs3/pdt/model/pdt_index')).
:- use_module(library('org/cs3/pdt/model/pdt_handle')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).



:- pdt_add_property_factory(builtin_predicate,builtin_predicate_factory).

lookup_handle(handle(id(Module:Name/Arity), builtin_predicate,Cache)):-
    pdt_index_load(builtin_predicates,Ix),
    pdt_index_get(Ix,Name,handle(id(Module:Name/Arity), builtin_predicate,Cache)).

get_property(_,_,_):-fail.

	
pdt_index_builtins:-
    forall(builtin(Module,Name,Arity),index_builtin(Module,Name,Arity)).
    
    
    
builtin(Module,Name,Arity):-    
	current_predicate(Name/Arity),
	functor(Head,Name,Arity),
	predicate_property(Head,built_in),
	user_imported_from(Head,Module).    
	
user_imported_from(Head,Module):-
	user:predicate_property(Head,imported_from(Module)),
	!.
user_imported_from(_,user).

index_builtin(Module,Name,Arity):-
    pdt_index_load(builtin_predicates,IX),
	index_builtin(Module,Name,Arity,IX,NextIX),
	pdt_index_store(builtin_predicates,NextIX).
	
index_builtin(Module,Name,Arity,IX,NextIX):-
	functor(Head,Name,Arity),
	findall(Prop,user:predicate_property(Head,Prop),Props),
	index_entry(Module:Name/Arity,[module(Module),name(Name),arity(Arity)|Props],Key,Value),
	pdt_index_put(IX,Key,Value,NextIX).
	
	
%index_entry(+Signature, +Props, -Key, -Value)
index_entry(Module:Name/Arity, Props,Name, handle(id(Module:Name/Arity), builtin_predicate,Props)).	
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


%%
% scopes are used to keep track of what is visible in a given context,
% e.g. a source file.
%
% A scope consists of 
% - a local namespace of bindings that are local to this scope
% - a set of contributions to non-local namespaces
% - a set of locally exported names
% - a set of globally exported names
%
% A scope X can import another scope Y. This means that
% - all locally exported names of scope Y are copied into the local namespace of X.
% - The same goes for globally exported names, but they are also added to the list of globally exported
%   names of X.
% - all non-local namespace contributions of Y are added to the contributions of X.
%
% An import may can also be partial. This is like a normal import, except that of the locally exported names
% only a sepcified subset is copied.
%
% A scope offers a starting point in name resolution. For a qualified name it will make sure that the 
% contributions override the original. Otherwise it will simply forward the search to the local namespace.
%
% A scope also offers hooks to specify conflict resolution strategies when naming conflicts are detected
% during the modification of a scopes namespaces.
:- module(pdt_scope,[
	pdt_scope_new/2,
	pdt_scope_resolve/3,
	pdt_scope_bind/4
]).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('org/cs3/pdt/util/pdt_util_set')).
:- use_module(library('org/cs3/pdt/model/pdt_namespace')).

:- pdt_define_context(scope(id,local_ns,contribs,local_xprts,global_xprts)).

pdt_scope_new(Id,SOut):-
    pdt_namespace_new(local_ns(Id),Ns),
    pdt_map_empty(Contribs),
    pdt_set_empty(LX),
    pdt_set_empty(GX),
    scope_new(S0),
    scope_set(S0,[
    	id=Id,
    	local_ns=Ns,
    	contribs=Contribs,
    	local_xprts=LX,
    	global_xprts=GX    	
    ],SOut).

pdt_scope_resolve(S,Name,Object):-
	scope_contribs(S,Contribs),    
    split_name(Name,Parent,Local),
    pdt_map_get(Contribs,Parent,Contrib),
    !,
    resolve_contrib(Contrib,S,Local,Object).
pdt_scope_resolve(S,Name,Object):-
	scope_local_ns(S,Ns),
	pdt_namespace_resolve(Ns,S,Name,Object).    

resolve_contrib(Ns,S,Name,Object):-
	pdt_namespace_resolve(Ns,S,Name,Object).

split_name(Fragment:Fragments,Fragment:Parent,Local):-
    functor(Fragments,:,2),
    !,
    split_name(Fragments,Parent,Local).
split_name(Parent:Local,Parent,Local).
    
pdt_scope_bind(Sin,Name,Object,Sout):-
	%if we can split the name, it is not local.
	split_name(Name,Parent,Local),
	!,
	%resolve the parent locally to find out if it is an indirect reference to our local ns
	(	pdt_scope_resolve(Sin,Parent,Key),
		pdt_namespace_for_object(Key,Ns),
		scope_local_ns(Sin,LocalNs),
		pdt_namespace_id(LocalNs,Id),
		pdt_namespace_id(Ns,Id)
	->	bind_local(Sin,Local,Object,Sout)
	;	bind_contrib(Sin,Parent,Local,Object,Sout)
	).
pdt_scope_bind(Sin,Name,Object,Sout):-	
	bind_local(Sin,Name,Object,Sout).
	
	
bind_local(Sin,Name,Object,Sout):-
	scope_local_ns(Sin,NsIn),
	resolve_conflict(Sin,NsIn,'',Name,Object,Result),
	(	Result=resolved(Resolution)
	->	pdt_namespace_bind(NsIn,Name,Resolution,NsOut),
		scope_set_local_ns(Sin,NsOut,Sout)
	;	throw(Result)
	).
	
bind_contrib(Sin,Parent,Local,Object,Sout):-
    ensure_contrib_exists(Sin,Parent,ContribIn,S1),
    resolve_conflict(S1,ContribIn,Parent,Local,Object,Result),
    	(	Result=resolved(Resolution)
	->	pdt_namespace_bind(ContribIn,Local,Resolution,ContribOut),
		scope_contribs(S1,ContribsIn),
		pdt_map_put(ContribsIn,Parent,ContribOut,ContribsOut),
		scope_set_contribs(S1,ContribsOut,Sout)
	;	throw(Result)
	).

ensure_contrib_exists(S,Parent,Contrib,S):-
    scope_contribs(S,Contribs),
    pdt_map_get(Contribs,Parent,Contrib),!.
ensure_contrib_exists(Sin,Parent,Contrib,Sout):-
    scope_contribs(Sin,Contribs),
	create_contrib(Sin,Parent,Contrib),
	pdt_map_put(Contribs,Parent,Contrib,ContribsNew),
	scope_set_contribs(Sin,ContribsNew,Sout).
	
create_contrib(Sin,Parent,ContribOut):-
    scope_id(Sin,Sid),
	pdt_namespace_new(contrib(Sid,Parent),Contrib0),
	pdt_namespace_add_base(Contrib0,lookup(Parent),start,ContribOut).
	
	
resolve_conflict(Sin,NsIn,Parent,Name,Object,Result):-
	pdt_namespace_resolve(NsIn,Sin,Name,OldObject),
	!,
	do_resolve_conflict(Sin,NsIn,Parent,Name,OldObject,Object,Result).    
resolve_conflict(_Sin,_NsIn,_Parent,_Name,Object,resolved(Object)).

do_resolve_conflict(Sin,_NsIn,Parent,Name,OldObject,Object,error(unresolved_nameing_conflict(Sin,Parent:Name,OldObject,Object))).	

pdt_namespace:pdt_namespace_for_object(lookup(Name),Context,Ns):-
    pdt_scope_resolve(Context,Name,Object),
    !,
    pdt_namespace_for_object(Object,Context,Ns).
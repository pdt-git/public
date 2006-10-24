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
% A namespace is a partial mapping from the set of names into the set of named objects.
%
% Names are terms of the form <term>:<term>: ... : <term>
% The terms between the colons are called fragments.
% A name with principal functor :/2 is a non-local name.
% All other terms are concidered as single fragments and this local names.
% There is no notion of absolute names, all names are only meaningfull relative to a given namespace.
%
% A name is resolved recursively by first resolving the leftmost fragment to an object. If the name has 
% any remaining fragments, the object will be regarded as some kind of reference to another namespace.
% pdt_namespace_for_object/2 is used to look it up. This other namespace is then used to
% resolve the remainder of the name recursively.
%
% A namespace consists of a set of local bindings, and an ordered list of base namespaces.
% 
% Local bindings are managed in a table and are used to resolve name fragments to objects.
%
% A base namespace is used as a fallback if a name fragment cannot be resolved. Thus, a namespace effectively
% inherits all bindings from its base namespaces. Note that a local binding of a name fragment always
% overrides any bindings to the same name in any of the base namespaces. 


:- module(pdt_namespace,[
	pdt_namespace_resolve/3,
	pdt_namespace_resolve/4,
	pdt_namespace_add_base/4,
	pdt_namespace_base/2,
	pdt_namespace_bind/4,
	pdt_namespace_for_object/2,
	pdt_namespace_for_object/3,	
	pdt_namespace_load/2,
	pdt_namespace_new/2,
	pdt_namespace_set_base/3,
	pdt_namespace_store/1,
	pdt_namespace_unstore/1,	
	pdt_namespace_unbind/3
]).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).

:- pdt_define_context(ns(id,local,base)).


pdt_namespace_new(Id,Ns):-
    ns_new(Ns),
    ns_id(Ns,Id),
    pdt_map_empty(Local),
    ns_local(Ns,Local),
    ns_base(Ns,[]).


%% pdt_namespace_resolve(+NameSpace, +Context, ?Name,?Object).
%
% succeeds if NameSpace binds Object to the name Name.
% Context is used to resolve found objects to other name spaces when resolving remote names.
% @see pdt_namespace_for_object/3
pdt_namespace_resolve(Ns,Cx,Name,Object):-
	resolve(Ns,Cx,Name,Object).
%% pdt_namespace_resolve(+NameSpace, ?Name,?Object).
% equivalent to pdt_namespace_resolve(NameSpace,[], Name,Object).
pdt_namespace_resolve(Ns,Name,Object):-
	resolve(Ns,[],Name,Object).


resolve(Ns,Cx,Fragment:Fragments,Object):-
    !,
    resolve_local(Ns,Cx,Fragment,X),
    pdt_namespace_for_object(X,Cx,NextNs),
    resolve(NextNs,Cx,Fragments,Object).
resolve(Ns,Cx,Fragment,Object):-
	resolve_local(Ns,Cx,Fragment,Object).
	
resolve_local(Ns,_Cx,Fragment,Object):-
	ns_local(Ns,Local),
	pdt_map_get(Local,Fragment,Object),
	!.
resolve_local(Ns,Cx,Fragment,Object):-
	ns_base(Ns,Base),
	resolve_base(Base,Cx,Fragment,Object).
	
resolve_base([NsId|_],Cx,Fragment,Object):-
    pdt_namespace_for_object(NsId,Cx,Ns),
    resolve_local(Ns,Cx,Fragment,Object),
    !.
resolve_base([_|NsIds],Cx,Fragment,Object):-
    resolve_base(NsIds,Cx,Fragment,Object).
    
%% pdt_namespace_bind(+NsIn, +Name, +Object, -NsOut)
% Add a new binding to a namespace.
%
% Only local bindings may be modified. If the name contains more than one fragment,
% an exception is thrown.
pdt_namespace_bind(NsIn,Name, Object,NsOut):-
    (	functor(Name,:,2)
    ->	throw(error(no_local_name(Name),bla))
    ;	ns_local(NsIn,LocalIn),
    	pdt_map_put(LocalIn,Name,Object,LocalOut),
    	ns_set_local(NsIn,LocalOut,NsOut)
    ).
    	

%% pdt_namespace_unbind(+NsIn, +Name, -NsOut)
% remove a binding from a namespace.
% Only local bindings may be modified. If the name contains more than one fragment,
% an exception is thrown.
pdt_namespace_unbind(NsIn,Name,NsOut):-
    (	functor(Name,:,2)
    ->	throw(error(no_local_name(Name),bla))
    ;	ns_local(NsIn,LocalIn),
    	pdt_map_remove(LocalIn,Name,LocalOut),
		ns_set_local(NsIn,LocalOut,NsOut)
    ).

%% pdt_namespace_base(+Ns,-Base)
% The second argument is unified with the base list of the namespace ns.
pdt_namespace_base(Ns,Base):-
	ns_base(Ns,Base).

%% pdt_namespace_set_base(+NsIn,+Base,-NsOut).
% Replace the fallback list of NsIn with Base and unify the result with NsOut.
pdt_namespace_set_base(NsIn,Base,NsOut):-
    ns_set_base(NsIn,Base,NsOut).

%% pdt_namespace_add_fallback(+NsIn,+FallBackId,+StartEnd,-NsOut).
% add a Namespace reference to the list of base namespaces of NsIn.
% @param StartEnd must be either start or end. It determines wether the 
% 	reference is added to the start or to the end of the list.
pdt_namespace_add_base(NsIn,FallBackId,start,NsOut):-
    !,
    ns_base(NsIn,BaseIn),
    ns_set_base(NsIn,[FallBackId|BaseIn],NsOut).
pdt_namespace_add_base(NsIn,FallBackId,end,NsOut):-
    ns_base(NsIn,BaseIn),
    append(BaseIn,[FallBackId],BaseOut),
    ns_set_base(NsIn,BaseOut,NsOut).

:-dynamic namespace/2.


%% pdt_namespace_load(+NsId,-NameSpace).
% Load the namespace with the id NsId from the heap and unify it with NameSpace
% if there is no namespace stored with this id, an empty one is created.
pdt_namespace_load(NsId,NameSpace):-
    namespace(NsId,NameSpace),
    !.
pdt_namespace_load(_NsId,NameSpace):-
	pdt_namespace_new(NameSpace).    
	
%% pdt_namespace_store(+NsId).	
% store a NameSpace on the heap.
%
% if another namespace is already stored with the same id, it is overwritten.
pdt_namespace_store(NameSpace):-
    ns_id(NameSpace,NsId),
    retractall(namespace(NsId,_)),
    assert(namespace(NsId,NameSpace)).

%% pdt_unstore(+NsId).	
% remove a namespace from heap
%
% if a namespace with the given id exists on the heap, it is removed. Otherwise, this predicate just succeeds.
pdt_namespace_unstore(NsId):-
    retractall(namespace(NsId,_)).



:- multifile pdt_namespace_for_object/3.
:- dynamic pdt_namespace_for_object/3.

%% pdt_namespace_for_object(+Object,+Context,-NameSpace)
%
% Hook predicate. Called during the resolution of qualified names to look up namespaces for 
% arbitrary objects.

% The default implementation covers native namespace terms aswell as namespace ids stored on the heap.
% (both ignoring the Context argument)
%
% @param Context can be used to provide additional information about the context in which the name should be 
% resolved. Add clauses to support namespace associations for custom objects.
pdt_namespace_for_object(Ns,_,Ns):-
	ns_new(Template),
    functor(Template,Name,Arity),
    functor(Ns,Name,Arity),!.
pdt_namespace_for_object(NsId,_,Ns):-
	namespace(NsId,Ns),!.
%%	pdt_namespace_for_object(+Object,-Context).
% equivalent to pdt_namespace_for_object(Objecte,[],Context).
pdt_namespace_for_object(Object,Context):-
	pdt_namespace_for_object(Object,[],Context).
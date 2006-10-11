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
%names are resolved as follows. First the name is split into parts delimited by the namespace separator ":".
% The leftmost part is resolved locally. If there are remaining parts, the resolved object is used to find 
% another namespace that is registered for that object. This namespace is used to recursively resolve the 
% remaining part of the name.
%
%
% Within the pdt, named objects are predicates and modules. (Variables may be added later)
% A predicate is a tuppel (Signature,Clauses,DefinitionModule) where
% - Signature is an unqualified predicate name
% - Clauses is a list of aterm references (i.e. elements point to "physical" terms in source files.)
% - DefinitionModule is a reference to a module definition.
%
% Namespace can be organized in fall-back hierarchies. Each namespace has a list of Namespace IDs that are 
% querried if a local search for a name fails. This way, the name bindings in the fall-back name spaces are
% effectively inherited in the local namespace. Inherited bindings can be overidden locally, if they are not 
% marked as final.
% 
% A module definition is a tupel (Name,Directive), where Directive is a reference to a physical directive 
% that constitutes the creation of a new module. Currently, only explicit :-module/2 directives are recognized.
%
% A (module-)qualified predicate name is a term of the form module:name/arity.
% An unqualified (also refered to as "local") predicate name is a term of the form name/arity. 
%
% Currently there is one namespace per source file + one namespace that binds system builtins. 
% 
:- module(pdt_namespace,[]).

pdt_namespace_new(ns(Local,Contrib,[])):-
	pdt_map_empty(Local),
	pdt_map_empty(Sub).

%% pdt_namespace_resolve(+NameSpace, ?Name,?Object).
%
% succeeds if NameSpace binds Object to the name Name.
pdt_namespace_resolve(NS,Name,Object):-
    
%% pdt_namespace_bind(+NsIn, +Name, +Object, -NsOut)
% Add a new binding to a namespace.
%
% If name is not local, sub-namespaces will be created as neccessary. 
% No external namespaces will be modified. 
pdt_namespace_bind(NsIn,Name, Object,NsOut).

%% pdt_namespace_unbind(+NsIn, +Name, -NsOut)
% remove a binding from a namespace.
% Non-local names will only be resolved in subnamespaces. 
% If the name is not bound locally, the predicate throws an exception.
% No external namespace will be modified. 
pdt_namespace_unbind(NsIn,Name,NsOut).

%% pdt_namespace_fallback_list(+Ns,-FallBacks)
% The second argument is unified with the fallback list of the namespace ns.
pdt_namespace_fallback_list(Ns,FallBacks).

%% pdt_namespace_set_fallback_list(+NsIn,+FallBacks,-NsOut).
% Replace the fallback list of NsIn with FallBacks and unify the result with NsOut.
pdt_namespace_set_fallback_list(NsIn,FallBacks,NsOut).

%% pdt_namespace_add_fallback(+NsIn,+FallBackId,+StartEnd,-NsOut).
% add 
pdt_namespace_add_fallback(NsIn,FallBackId,StartEnd,NsOut).

pdt_namespace_load(NsId,NameSpace).
pdt_namespace_store(NsId,NameSpace).

pdt_namespace_for_object(Object,NsId).
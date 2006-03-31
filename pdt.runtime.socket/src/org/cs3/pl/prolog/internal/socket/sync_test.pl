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

:- multifile test/1.
:- multifile tearDown/1.

:- [magicmap].
:- [sync].
:- [localisation].

test(separate_functor_arity_module_safe):-
    sync:separate_functor_arity_module_safe(m:f/1, m:f, 1),
    sync:separate_functor_arity_module_safe('m:f/1', m:f, 1),
    sync:separate_functor_arity_module_safe(f/2, f, 2).
    
test(evaluate_and_store_idb):-
    sync:evaluate_and_store_idb(mymodule:non_existing(_t)),
    clause(sync:idb(mymodule:non_existing(_A)),fail).

tearDown(evaluate_and_store_idb):-
    retractall(sync:idb(_)).


test(update_idb_and_changed):-
    Term = testmodule:newpred(_A,b),
    term_to_atom(Term, Atom),
    sync:init_idb(Atom),
    assert(testmodule:newpred(a,b)),
    sync:term_ref(Term,Ref),
	sync:update_idb(Term, Ref),
	testmodule:newpred(a,b),
    clause(sync:idb_copy(Ref,testmodule:newpred(_,_)),fail),
    sync:changed(Ref).

tearDown(update_idb_and_changed):-
	sync:term_ref(testmodule:newpred(_A,b), Ref),
	sync:remove_term_idb(Ref),
	retractall(testmodule:newpred(_,_)).
    
tearDown('update_idb and changed'):-
    retractall(sync:idb(_)),
    retractall(sync:idb_copy(_)),
    retractall(testmodule:newpred(_,_)).

setUp(term_ref) :-
    sync:init_term_ref(term(a,_B),_Ref).
    	
test(term_ref):-
    assert_true(sync:term_ref(term(a,_C),_Ref)).
    
tearDown(term_ref):-
	sync:term_ref(term(a,_C),Ref),
	sync:remove_term_idb(Ref).

setUp(init_idb):-
    assert(magicmap:location('00-09-2D-53-27-3A', 50.73, 7.12, 0)),
    retractall(sync:idb(_,_)).
    
test(init_idb) :-
  sync:init_idb('localisation:company_nearby(_,_,_,1000)'),
  sync:term_ref(localisation:company_nearby(_,_,_,1000), Ref),


  retract(magicmap:location('00-09-2D-53-27-3A', 50.73, 7.12, 0)),
  assert(magicmap:location('00-09-2D-53-27-3A', 50.73, 7.123, 0)),

  sync:update_idb(localisation:company_nearby(_,_,_,1000), Ref),

  sync:changed(Ref).
  
tearDown(init_idb):-
    retractall(magicmap:location(_,_,_,_)),
    sync:term_ref(localisation:company_nearby(_,_,_,1000), Ref),
    sync:remove_term_idb(Ref).
 
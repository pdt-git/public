/*implement the source_term interface using pefs

terms are backed by pefs on the heap. each term has an ID, 
aswell as a list of changes affecting it.
*/

:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_source_term')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).

:- pdt_define_context(pefterm(id, facts)).


pdt_source_term:implements_source_term.

source_term_hook(T,pdt_pef_term):-
    non_var(T),
    pefterm_new(T).
    
source_term_expand_hook(T,Exp):-
    pef_term_expand(T,Exp).
	
source_term_functor_hook(T,Name,Arity):-
	pef_term_functor(T,Name,Arity).
	
source_term_arg_hook(ArgNum,T,ArgVal):-
	pef_term_arg(ArgNum,T,ArgVal).

source_term_property_hook(T,Key,Value):-
	pef_term_property(T,Key,Value).
	
source_term_set_property_hook(T,Key,Value,TT):-	
	pef_term_set_property(T,Key,Value,TT).

source_term_all_properties_hook(_T,_Props):-
   !,fail.

source_term_copy_properties_hook(_From,_To,_Out):-	
	!,fail.
	
source_term_create_hook(Term,PefTerm):-
    pef_term_create(Term,PefTerm).
    
source_term_var_hook(T):-
    pef_term_var(T).

pef_term_create(Term,PefTerm):-
    var(Term),
    !,
    
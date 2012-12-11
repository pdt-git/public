
:- module(pdt_support, [pdt_support/1]).

:- if(current_prolog_flag(dialect,swi)).
	pdt_support(doc_collect).
	pdt_support(flag).
	pdt_support(count_inferences).
	pdt_support(tests).
	pdt_support(clause_property).
	
:- elif(current_prolog_flag(dialect,yap)).
	pdt_support(reverse_list).
	
:- else.
	:- writeln('WARNING: unsupported Prolog dialect!\nSupported dialects are: SWI, YAP').
	
:- endif.



:- module(pdt_support, [pdt_support/1, doc_collect/1]).

:- if(current_prolog_flag(dialect,swi)).
	pdt_support(doc_collect).
	pdt_support(flag).
	pdt_support(count_inferences).
	pdt_support(tests).
	pdt_support(clause_property).
	pdt_support(last_call_optimisation).
	
:- elif(current_prolog_flag(dialect,yap)).
	pdt_support(reverse_list).
	pdt_support(table).
	pdt_support(remove_duplicates).
	
:- else.
	:- writeln('WARNING: unsupported Prolog dialect!\nSupported dialects are: SWI, YAP').
	
:- endif.


:- if(\+ pdt_support(doc_collect)).
doc_collect(_) :-
	writeln('WARNING: doc_collect not supported in current prolog version').
:- endif.


% pdt_support(last_call_optimisation).
%     current_prolog_flag(last_call_optimisation, X) is supported

% pdt_support(table).
%     tabling (and the table/1 directive) is supported
%     prints warning if not supported
	
% pdt_support(doc_collect).
%     doc_collect/1 is supported.
%     prints warning if not supported

% pdt_support(flag).
%     flag/3 is supported
%     alternative implementation is used if not supported

% pdt_support(tty_control).
%     current_prolog_flag(tty_control, X) is supported

% pdt_support(remove_duplicates).
%     remove_duplicates/2 is supported
%     alternative implementation is used if not supported

% pdt_support(count_inferences)
%	  statistics(inferences, I) is supported
   

% pdt_support(tests).

% pdt_support(clause_property).
	
% pdt_support(reverse_list).
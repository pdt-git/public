% Author: Tobias
% Date: 06.02.2003

:- dynamic errorhandling/0.
:- dynamic halt_on_error/0.

% temporary necessary while aspects are not 
% completely represented in prolog factbase:
:- dynamic slAspectT/4. 
:- multifile slAspectT/4.

error_handling_add_error_term(_,_term, _) :-
    call(_term),
    !.
error_handling_add_error_term(SourceLocation,_, Err) :-
    %term_to_atom(_term,_err),
    sformat(ErrorString, 'err ~w',[Err]),
    write(ErrorString),
    addErrorFacts(SourceLocation,Err),  
    flush_output,
    debugme,
    halt_on_error ->
	    halt;
	    throw(ErrorString).
    
error_handling(_term, _,_) :-
    call(_term),
    !.
error_handling(_,_format,_term) :-
    term_to_atom(_term,_atom),
    sformat(_err,_format,[_atom]),
    error_handling_add_error_term(null,fail,_err).

addErrorFacts(null,_).
addErrorFacts(sourceLocation(File, Start, Length),Err):-
   new_id(ID),
   add(isErrorWarningMessage('declare error', ID, Err)), 
   add(slAspectT(ID, File,Start, Length)).
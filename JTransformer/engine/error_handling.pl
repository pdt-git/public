% Author: Tobias
% Date: 06.02.2003

:- dynamic errorhandling/0.
:- dynamic halt_on_error/0.

error_handling(_term, _) :-
    call(_term),
    !.
error_handling(_,_term) :-
    term_to_atom(_term,_err),
    sformat(ErrorString, 'err ~a',[_err]),
    write(ErrorString),
    flush_output,
    halt_on_error ->
	    halt;
	    throw(ErrorString).
    
error_handling(_term, _,_) :-
    call(_term),
    !.
error_handling(_,_format,_term) :-
    term_to_atom(_term,_atom),
    sformat(_err,_format,[_atom]),
    error_handling(fail,_err).


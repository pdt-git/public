% Author: Tobias
% Date: 06.02.2003

:- dynamic errorhandling/0.

error_handling(_term, _) :-
    call(_term),
    !.
error_handling(_,_term) :-
    term_to_atom(_term,_err),
    format('err ~a',[_err]),
    flush_output,
    halt.
    
error_handling(_term, _,_) :-
    call(_term),
    !.
error_handling(_,_format,_term) :-
    term_to_atom(_term,_atom),
    sformat(_err,_format,[_atom]),
    error_handling(fail,_err).


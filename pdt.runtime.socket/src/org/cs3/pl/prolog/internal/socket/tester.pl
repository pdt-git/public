%call_if_not_cut(OutStream, Goal):-
%    user:Goal,
%    handle_batch_messages,
%	(	recorded(pif_batch_abort,_)
%	->	my_format(OutStream, "CUT~n",[]),
%		fail
%	;	true
%	).
%	
%handle_batch_messages:-
%    repeat,    
%    (	thread_peek_message(Message),handle_batch_message(Message)
%    ->	thread_get_message(Message), fail
%    ;	true
%    ),!.
%handle_batch_message(abort(Id)):-
%	recordz(pif_batch_abort,Id).	
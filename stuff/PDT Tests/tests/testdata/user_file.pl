predicate_one.

:- multifile(user:message_hook/3).

user:message_hook(_,_,_) :- fail.
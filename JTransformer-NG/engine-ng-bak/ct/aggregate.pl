apply_encapsulate_fields(Name):-
    apply_ctlist([aiget(Name),aiset(Name),acget(Name),acset(Name)]).
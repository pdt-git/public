orig_create(Input,Term).
orig_lookup(Fileref,Termref,Term).
orig_store(Term).
orig_arg(ArgNum,Term,ArgVal).
orig_functor(Term,Name,Arity).
orig_property(Term,Prop,Val).
orig_set_property(Term0,Prop,Val,Term1).

new_create(Input,TID).
new_lookup(Fileref,Termref,TID).
new_store(TID).
new_arg(ArgNum,TID,ArgID).
new_functor(TID,Name,Arity).
new_property(TID,Prop,Val).
new_set_property(TID,Prop,Val).


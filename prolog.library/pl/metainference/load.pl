:- [pmi].
:- [meta_inference_examples].
:- [meta_inference_test].

/* 
declared_in_module(Module, Name, Arity, Module), functor(Head, Name, Arity), infer_meta(Module:Head, MetaSpec), infer_meta_predicate(Module:Head, MetaSpec2), MetaSpec \== MetaSpec2.

pmi:inferred_meta_pred2(_, M, MetaSpec), predicate_property(M:MetaSpec, file(File)), atom_concat('l:/work/beckera/git-repos/jtransformer', _, File), \+ predicate_property(M:MetaSpec, meta_predicate(_)).
*/

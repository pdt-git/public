:- ensure_loaded('pef_base__term_expansion-v3').
:- op(600,xfy,@).
'$option'(implicit_ids).
'$option'(index_foreign_keys).

data((:- define_pef(
	pef_imported_predicate(
		name @label @index,
		arity @label,
		module:module, %importing module		
		from_name @label
	) @weak @edge @composite_index([name,module])
))).
/*
:- define_pef(
	pef_imported_predicate(
		name @label @index,
		arity @label,
		module:module, %importing module		
		from_name @label
	) @weak @edge @composite_index([name,module])
).
*/
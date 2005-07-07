:- style_check(-atom).
:- user:assert(meta_data_module('test002','user', "")).
:- user:assert(meta_data('test002',user,'+',2,true,0,6,false,false)).
:- user:assert(meta_data('test002',user,'+',2,true,8,8,false,false)).

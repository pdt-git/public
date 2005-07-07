:- style_check(-atom).
:- user:assert(meta_data_module('test000','user', "")).
:- user:assert(meta_data('test000',user,'gut',1,true,0,8,false,false)).
:- user:assert(meta_data('test000',user,'gut',1,true,10,8,false,false)).
:- user:assert(meta_data('test000',user,'gut',1,true,20,8,false,false)).

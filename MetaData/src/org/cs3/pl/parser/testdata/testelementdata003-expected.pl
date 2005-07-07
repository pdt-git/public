:- style_check(-atom).
:- user:assert(meta_data_module('test003','second', "")).
:- user:assert(meta_data('test003',second,'+',2,true,30,6,false,false)).
:- user:assert(meta_data('test003',second,'+',2,true,38,8,false,false)).
:- user:assert(meta_data('test003',second,'a',0,true,48,1,false,false)).

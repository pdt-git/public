:- module(repmac,[]).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).

:- pdt_define_context(
	cx(
		out, %output stream
   		in, %stream reading from src (memory) file
   		null, %null stream for discarding skipped chars.
   		buffer, %memory file
   		buffer_out, %stream writing to buffer
   		node %the ast node we are currently processing
   	)
).

s([],Cx,Cx).
s([copy(N)|Ns],CxIn,CxOut):-
	buf_clear(CxIn,Cx1),
	c([N],Cx1,Cx2),
	src_position(Cx2,P),
	add_replace(P,P,Cx2,Cx3),
	s(Ns,Cx2,CxOut).
s([replace(t(T,I,J))|Ns],CxIn,CxOut):-
	src_position(CxIn,P),
	buf_clear(CxIn,Cx1), add_replace(P,I,Cx1,Cx2),
	buf_append(T,Cx2,Cx3), add_replace(I,J,Cx3,Cx4),
	src_seek(Cx4,J,Cx5),
	s(Ns,Cx5,CxOut).
s([n(N)|Ns],CxIn,CxOut):-
	children(N,CxIn,ChildNode,Cs),
	cx_node(CxIn,Node);
	cx_set_node(CxIn,ChildNode,CxChild),
	s(Cs,CxChild,CxChildOut),
	cx_set_node(CxChildOut,Node,CxNext),
	s(Ns,CxNext,CxOut).
s([t(_,I,J)|Ns],CxIn,CxOut):-
    src_position(CxIn,P),
	buf_clear, add_replace(P,I,CxIn,Cx1),
	src_seek(Cx1,J,CxNext),
	s(Ns,CxNext,CxOut).
	
	
c([],Cx,Cx).
c([copy(N)|Ns],CxIn,CxOut):-
	c([N|Ns],CxIn,CxOut).
c([replace(t(T,_,_))|Ns],CxIn,CxOut):-
	buf_append(T,CxIn,CxNext),
	c(Ns,CxNext,CxOut).
c([n(N)|Ns],CxIn,CxOut):-
	children(N,CxIn,ChildNode,Cs),
	cx_node(CxIn,Node),
	cx_set_node(CxIn,ChildNode,CxChild), 
	c(Cs,CxChild,CxChildOut),
	cx_set_node(CxChildOut,Node,CxNext),
	c(Ns,CxNext,CxOut).
c([t(T,_,_)|Ns],CxIn,CxOut):-
	buf_append(T,CxIn,CxNext),
	c(Ns,CxNext,CxOut).


src_position(Cx,Pos):-
    cx_in(Cx,In),
    character_count(In,Pos).
    
src_seek(Cx,NewPos,Cx):-
    cx_null(CxIn,Null),
    cx_in(CxIn,In),
    src_position(CxIn,CurrentPos),
    Skip is NewPos - CurrentPos,
    copy_stream_data(In,Null,Skip).
	
%% add_replace(+Start,+End,+CxIn,-CxOut).
% generate a replace instruction
% replace(Start,End,String),
% where String is the current content of the buffer.
% The buffer is cleared after this operation.
add_replace(I,J,CxIn,CxOut):-
    cx_buffer(CxIn,Buf),
    cx_buffer_out(CxIn,BufOut),
    close(BufOut),    
    memory_file_to_codes(Buf,Codes),
    open_memory_file(Buf,Write,NewBufOut),
    cx_set_buffer_out(CxIn,NewBufOut,CxNext),
    string_to_codes(String,Codes),    
	output(replace(I,J,String),CxNext,CxOut).
	
	

buf_append(I,J,CxIn,CxOut):- %not needed?
    cx_in(CxIn,In),
    cx_buffer_out(CxIn,BufOut),
    src_seek(CxIn,I,Cx1),
    Skip is J - I,
    copy_stream_data(In,BufOut,Skip).

buf_append(Codes,Cx,Cx):-
	cx_buffer_out(Cx,BufOut),
	format(BufOut,"~s", [Codes]).  


buf_clear(Cx,Cx).%not needed?	


ast_tokens(Node,Tokens):-
	(	pef_property_query([pef=Node,key=tokens,value=Tokens0])
	->	process_ast_tokens(Tokens0,Tokens)
	;	Tokens=[]
	).	

process_ast_tokens([],_,[]).
process_ast_tokens([token(Type,From,To)|Tokens0],Node,[TokenOut|Tokens]):-
    (	Type=argument(I)
    ->	Token=n(I)
    ;	Type==functor
    ->	pef_term_query([id=Node,name=FunctorName]),
    	atom_codes(FunctorName,Codes),
    	(	pef_property_query([pef=Node,key=replace,value=true])
    	->	Token=replace(t(Codes,From,To))
    	;	Token=t(Codes,From,To)
    	)
    ;	Type==gap
    ->	Token=t([],From,To)
    ;	Type==simple
    ->	(	pef_variable_occurance_query([id=Node,variable=Var])
    	->	pef_variable_query([id=Var,name=SimpleName])
    	;	pef_term_query([id=Node,name=SimpleName])
    	),
    	atom_codes(SimpleName,Codes),
    	(	pef_property_query([pef=Node,key=replace,value=true])
    	->	Token=replace(t(Codes,From,To))
    	;	Token=t(Codes,From,To)
    	)
    ;	throw(bad_token(token(Type,From,To)))
    ),
    (	pef_property_query([pef=Node,key=copy,value=true])
    	->	TokenOut=copy(Token)
    	;	TokenOut=Token
    ),
    process_ast_tokens(Tokens0,Node,Tokens).
    
    
	
children(N,CxIn,ChildNode,Cs):-
	cx_node(CxIn,Node),
	pef_arg_query([num=N,parent=Node,child=ChildNode]),
	ast_tokens(ChildNode,Cs).
:- module(delta,[]).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/tokens')).
:- use_module(library('util/layout')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- ensure_loaded(library('org/cs3/pdt/util/pdt_util_term_position')).

pdt_builder:build_hook(delta):-
    build_resource_delta,
    forall(
    	pef_modified_toplevel_query([toplevel=Tl]),
    	build_text_delta(Tl)
    ).


build_resource_delta:-
    forall(
    	(	pef_renamed_file_query([file=File,old_path=OldPath]),
    		pef_file_query([id=File,path=NewPath])
    	),
    	(	file_directory_name(NewPath,NewDir),
	    	file_directory_name(OldPath,OldDir),
	    	(	OldDir==NewDir
	    	->	file_base_name(NewPath,NewName),
	    		pef_resource_delta_assert([old_path=OldPath,new_path=NewName,type=rename])
	    	;	pef_resource_delta_assert([old_path=OldPath,new_path=NewPath,type=move])
	    	)
	    )    	
    ).
build_text_delta(Tl):-    
    pef_ast_query([toplevel=Tl,root=Node]),
    pef_toplevel_query([id=Tl,file=File,positions=Pos]),
    top_position(Pos,Offset,_),    
    cx_new(Cx),
    cx_file(Cx,File),    
    layout:get_memory_file(File,MemFile),
    open_memory_file(MemFile,read,In),    
    cx_in(Cx,In),
    open_null_stream(Null),
    cx_null(Cx,Null),
    cx_out(Cx,Null),
    new_memory_file(Buffer),
    cx_buffer(Cx,Buffer),
    open_memory_file(Buffer,write,BufferOut),
    cx_buffer_out(Cx,BufferOut),
    cx_node(Cx,Node),
    src_seek(Cx,Offset,Cx1),
    ast_tokens(Node,Tokens),
    s(Tokens,Cx1,Cx2),
    cx_null(Cx2,Null2),
    cx_in(Cx2,In2),
    cx_buffer_out(Cx2,BufferOut2),
    close(Null2),
    close(In2),
    close(BufferOut2).


	
:- pdt_define_context(
	cx(
		out, %output stream
   		in, %stream reading from src (memory) file
   		null, %null stream for discarding skipped chars.
   		buffer, %memory file
   		buffer_out, %stream writing to buffer
   		buffer_empty, %flag to indicate whether the buffer is empty.
   		node, %the ast node we are currently processing
   		file 
   	)
).


gen_delta(Node,Offset,In):-
    ast_toplevel(Node,Tl),
    pef_toplevel_query([id=Tl,file=File]),    
    cx_new(Cx),
    cx_file(Cx,File),    
    cx_in(Cx,In),
    open_null_stream(Null),
    cx_null(Cx,Null),
    cx_out(Cx,Null),
    new_memory_file(Buffer),
    cx_buffer(Cx,Buffer),
    open_memory_file(Buffer,write,BufferOut),
    cx_buffer_out(Cx,BufferOut),
    cx_node(Cx,Node),
    src_seek(Cx,Offset,Cx1),
    ast_tokens(Node,Tokens),
    s(Tokens,Cx1,_).
s([],Cx,Cx):-
    !.
s([copy(N)|Ns],CxIn,CxOut):-
    !,
	buf_clear(CxIn,Cx1),
	cx_buffer_out(Cx1,Out),
	layout_node(N,Out),
	cx_set_buffer_empty(Cx1,false,Cx2),
	src_position(Cx2,P),
	add_replace(P,P,Cx2,Cx3),
	s(Ns,Cx3,CxOut).
s([replace(t(T,I,J))|Ns],CxIn,CxOut):-
	!,
	src_position(CxIn,P),
	buf_clear(CxIn,Cx1), add_replace(P,I,Cx1,Cx2),
	buf_append(T,Cx2,Cx3), add_replace(I,J,Cx3,Cx4),
	src_seek(Cx4,J,Cx5),
	s(Ns,Cx5,CxOut).
s([n(N)|Ns],CxIn,CxOut):-
	!,
	children(N,CxIn,ChildNode,Cs),
	cx_node(CxIn,Node),
	cx_set_node(CxIn,ChildNode,CxChild),
	s(Cs,CxChild,CxChildOut),
	cx_set_node(CxChildOut,Node,CxNext),
	s(Ns,CxNext,CxOut).
s([t(_,I,J)|Ns],CxIn,CxOut):-
    src_position(CxIn,P),
	buf_clear(CxIn,Cx1), add_replace(P,I,Cx1,Cx2),
	src_seek(Cx2,J,CxNext),
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
    cx_null(Cx,Null),
    cx_in(Cx,In),
    src_position(Cx,CurrentPos),
    Skip is NewPos - CurrentPos,
    copy_stream_data(In,Null,Skip).


%% add_replace(+Start,+End,+CxIn,-CxOut).
% generate a replace instruction
% replace(Start,End,String),
% where String is the current content of the buffer.
% The buffer is cleared after this operation.
add_replace(I,J,CxIn,CxOut):-
    (	I==J, 
    	cx_buffer_empty(CxIn,true)
    ->	CxOut=CxIn
    ;   cx_buffer(CxIn,Buf),
	    cx_buffer_out(CxIn,BufOut),
	    close(BufOut),	        	   
	    memory_file_to_codes(Buf,Codes),
	    open_memory_file(Buf,write,NewBufOut),
	    cx_set_buffer_out(CxIn,NewBufOut,CxOut),
	    string_to_list(String,Codes),    
		cx_file(CxOut,File),
		pef_text_delta_assert([file=File,start=I,end=J,text=String])
	).


buf_append(I,J,CxIn,CxOut):- %not needed?
    cx_in(CxIn,In),
    cx_buffer_out(CxIn,BufOut),
    src_seek(CxIn,I,Cx1),
    cx_set_buffer_empty(Cx1,false,CxOut),
    Skip is J - I,
    copy_stream_data(In,BufOut,Skip).

buf_append(Codes,Cx,Cx1):-
	cx_buffer_out(Cx,BufOut),
	cx_set_buffer_empty(Cx,false,Cx1),
	format(BufOut,"~s", [Codes]).  


buf_clear(Cx,Cx1):-
	cx_set_buffer_empty(Cx,true,Cx1).	


ast_tokens(Node,Tokens):-
	(	pef_property_query([pef=Node,key=copy,value=true])
	->	Tokens=[copy(Node)]		
	;	pef_property_query([pef=Node,key=tokens,value=Tokens0])
	->	process_ast_tokens(Tokens0,Node,Tokens)
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
	!,
	ast_tokens(ChildNode,Cs).

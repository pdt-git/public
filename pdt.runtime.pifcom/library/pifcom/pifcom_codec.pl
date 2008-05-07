:- module(pifcom_codec,
	[	pifcom_read_message/4,
		pifcom_write_message/4,
		pifcom_read_and_decode_message/4,
		pifcom_encode_and_write_message/4,		
		pifcom_encode_body/2,
		pifcom_decode_body/2,
		pifcom_write_integer_bytes/3
	]
).

:- use_module(library(record)).
:- use_module(library(memfile)).



%% pifcom_read_and_decode_message(+Stream,-OpCode,-Ticket,-Data).
% read and decode a pifcom message.
%
% This is a convenience predicate using pifcom_read_message/4 and
% pifcom_decode_body/3. It tries to guess the body type from looking
% at the opcode. If the body type cannot be determined this way, or
% if the body type is not supported by the codec, this predicate fails
% silently.
pifcom_read_and_decode_message(Stream,OpCode,Ticket,Data):-
    pifcom_read_message(Stream,OpCode,Ticket,Body),
    call_cleanup(
    	(	opc(OpCode,_,Data),
    		catch(pifcom_decode_body(Data,Body),E,throw(error(pifcom_error(E,decode_body(OpCode,Ticket)))))
    	),
    	free_memory_file(Body)
    ).

%% pifcom_encode_and_write_message(+Stream,+OpCode,+Ticket,+Data).
% encode and write a pifcom message.
%
% This is a convenience predicate using pifcom_write_message/4 and
% pifcom_encode_body/3. It tries to guess the body type from looking
% at the opcode. If the body type cannot be determined this way, or
% if the body type is not supported by the codec, this predicate fails
% silently.
pifcom_encode_and_write_message(Stream,OpCode,Ticket,Data):-
 	pifcom_encode_body(Data,Body),
    call_cleanup(
    	pifcom_write_message(Stream,OpCode,Ticket,Body),
    	(	Body==[]
    	->	true
    	;	free_memory_file(Body)
    	)
    ).


%% pifcom_read_message(+Stream, -Operation, -Ticket, -Body).
% read a pifcom message from an octet stream.
%
% The message body is read into a memory file, a handle to this file is unified with Body.
% The message may be spread over several message fragments. This predicate automatically detects this
% and concatenates the body fragments.
% The format of the body data depends on the message opcode. This predicate makes
% no attempt to interprete the data. 
%
% @param Stream the octet stream to read from 
% @param OpCode an atom describing the message intention
% @param Ticket the ticket this message refers to
% @param Body a memory file containing the raw data of the message body.
%


close_memfile(Out,MF):-
	byte_count(Out,C),
	flush_output(Out),    
	byte_count(Out,C),
	close(Out),
	size_memory_file(MF,S),
	size_memory_file(MF,S).
	
pifcom_read_message(Stream,OpCode,Ticket,Body):-
    new_memory_file(Body),
    open_memory_file(Body,write,Out,[encoding(octet)]),
    set_stream(Out,encoding(octet)),
    call_cleanup(
    	read_message(Stream,OpCode,Ticket,Out),
    	close_memfile(Out,Body)
    ).
    
%% pifcom_write_message(+Stream, +OpCode, +Ticket, +Body).
% write a pifcom message to an octet stream.
%
% The message body must be provided in a memory file. If the body is to large
% to fit into a single message, it is split into several continuation messages.
%
% @param Stream the octet stream to read from 
% @param OpCode an atom describing the message intention
% @param Ticket the ticket this message refers to
% @param Body a memory file containing the raw data of the message body.

pifcom_write_message(Stream,OpCode,Ticket,Body):-
    write_message(Stream,OpCode,Ticket,Body).



%% pifcom_encode_body(+Data,-Body).
% encode data to a pifcom message body.
% Currently the following data formats are supported:
%
%  name(Name) - a single variable name.
%  cterm(VarNames,Term) - a canonical representation of Term, using UTF-8 encoding.
%                         VarNames is a list of Name=Var pairs which can be used to specify Variablenames.
% uint(Integer) - represent Integer as 4-byte unsigned integer.
% 
% @param Data the data to encode.
% @param Body a memfile containing the encoded body.
pifcom_encode_body(Data,Body):-
    encode_body(Data,Body).
    	

%% pifcom_decode_body(+Template,+Body).
% decode a pifcom message body.
% 
% See pifcom_encode_body/2 for supported encodings.
%
% @param Template a term of one of the following forms:
%        name(Name), cterm(VarNames,Term), uint(Integer)
% @param Body a memfile containing the encoded body.
pifcom_decode_body(Template,Body):-	
	decode_body(Template,Body).


:- record( header(flags=[],op=0,ticket=0,body_len=0)).

:- discontiguous opc/3,flc/2.

% some convenience "macros" for dealing with header fields.

user:term_expansion(opc(A,B,C),[opc(A,B,C),opc(B,A,C)]).
user:term_expansion(flc(A,B),[flc(A,B),flc(B,A),(Head:-A is A /\ Type)]):-
	Head=..[B,Type].


opc(0x0, mark, []).
opc(0x1, skip, []).
opc(0x2, cut, []).
opc(0x3, abort, []).
opc(0x4, complete, []).
opc(0x5, empty, []).
opc(0x6, fail, []).
opc(0x7, bye, []).
opc(0x8, query, cterm(_,_)).
opc(0x9, begin_names, uint(_)).
opc(0xA, begin_solution, uint(_)).
opc(0xB, error, cterm(_,_)).
opc(0xC, name, name(_)).
opc(0xD, binding, cterm(_,_)).
opc(0xE, [], []).
opc(0xF, protocol_error, cterm(_,_)).

flc(0x08,body).
flc(0x10,reserved1).
flc(0x20,reserved2).
flc(0x40,incomplete).
flc(0x80,continuation).


type_opc(Type,OpC):-
    OpC is Type /\ 0xF.

type_op(Type,Op,Body):-
	type_opc(Type,OpC),
	opc(OpC,Op,Body).


sum_flags(Flags,Sum):-
    sum_flags(Flags,0,Sum).

sum_flags([],Sum,Sum).
sum_flags([Flag|Flags],In,Sum):-
    flc(Flag,C),
    Next is In + C,
    sum_flags(Flags,Next,Sum).
    

calculate_type(Op,Flags,Type):-
    opc(Op,OpC,_),
    sum_flags(Flags,OpC,Type).

write_header(Stream,Header):-
    make_header([flags(Flags),op(Op),ticket(Ticket),body_len(BodyLen)],Header),
    calculate_type(Op,Flags,Type),    
	put_byte(Stream,Type),
	(	ommit_ticket(Type)
	->	true
	;	write_ticket(Stream,Ticket)
	),
	(	ommit_length(Type)
	->	true
	;	write_length(Stream,BodyLen)
	).

ommit_ticket(Type):-
	continuation(Type).
	
ommit_length(Type):-
	\+ body(Type),
	!.	
ommit_length(Type):-
	incomplete(Type),
	!.	

read_header(Stream,Header):-
    make_header([flags(Flags),op(Op),ticket(Ticket),body_len(BodyLen)],Header),
	get_byte(Stream,Type),	
	type_op(Type,Op,_),
	(	continuation(Type)
	->	Flags0=[continuation],
		Ticket=[]
	;	Flags0=[],
		read_ticket(Stream,Ticket)
	),
	(	incomplete(Type)
	->	Flags=[incomplete|Flags0],		
		(	body(Type)
		->	BodyLen=0xFFFF
		;	throw(incomplete_and_bodyless)
		)
	;	Flags=Flags0,
		(	body(Type)
		->	read_length(Stream,BodyLen)
		;	BodyLen=0
		)
	).



read_ticket(Stream,Ticket):-
    read_bytes_to_integer(Stream,2,Ticket).
    
read_length(Stream,Length):-
	read_bytes_to_integer(Stream,2,Length).
	
write_ticket(Stream,Ticket):-
	write_integer_bytes(Stream,2,Ticket).

write_length(Stream,Length):-
	write_integer_bytes(Stream,2,Length).


read_bytes_to_integer(Stream,Len,Integer):-
    read_bytes_to_integer_X(Len,Stream,0,Integer).
    
read_bytes_to_integer_X(0,_Stream,Old,Sum):-
    !,
    Old=Sum.
read_bytes_to_integer_X(I,Stream,Old,Sum):-
	get_byte(Stream,Byte),
    J is I - 1,
	New is Old + (Byte << (J * 8)),
	read_bytes_to_integer_X(J,Stream,New,Sum).


pifcom_write_integer_bytes(Stream,Len,Integer):-
    put_integer_bytes_X(Integer,Len,Stream).
    
write_integer_bytes(Stream,Len,Integer):-
    put_integer_bytes_X(Integer,Len,Stream).

put_integer_bytes_X(_T,0,_Out):-
    !.
put_integer_bytes_X(T,D,Out):-
    E is D - 1,
    Byte is T >> (E * 8),
    put_byte(Out,Byte),
    Remain is T - (Byte <<(E * 8)),
    put_integer_bytes_X(Remain,E,Out).



	
test_header(Header,Header2):-
    new_memory_file(MF),
	open_memory_file(MF,write,Out,[encoding(octet)]),
    set_stream(Out,encoding(octet)),	
	write_header(Out,Header),
	close(Out),
	open_memory_file(MF,read,In,[encoding(octet)]),
    set_stream(In,encoding(octet)),	
	read_header(In,Header2),
	close(In),
	free_memory_file(MF).	
	


read_message(Stream,Op,Ticket,OutStream):-
	read_header(Stream,Header),
	make_header([flags(Flags),op(Op),ticket(Ticket),body_len(BodyLen)],Header),
	(	memberchk(continuation,Flags)
	->	throw(message_starts_with_continuation)
	;	copy_stream_data(Stream,OutStream,BodyLen),
		read_message_continuation(Flags,Stream,OutStream)
	).

read_message_continuation(Flags,Stream,OutStream):-	
	memberchk(incomplete,Flags),
	!,
	read_header(Stream,Header),
	header_flags(Header,NextFlags),
	(	memberchk(continuation,NextFlags)
	->	true
	;	throw(expected_continuation)
	),
	header_body_len(Header,BodyLen),
	byte_count(OutStream,Before),
	copy_stream_data(Stream,OutStream,BodyLen),
	byte_count(OutStream,After),	
	Before\==After,	
	read_message_continuation(NextFlags,Stream,OutStream).
read_message_continuation(_,_,_).

write_message(Stream,Op,Ticket,MemFile):-
	(	MemFile==[]
	->	Size=0,
		write_message_X(0,Stream,Op,Ticket,[])
	;	size_memory_file(MemFile,Size),
    	open_memory_file(MemFile,read,InStream,[encoding(octet)]),
    	set_stream(InStream,encoding(octet)),
    	call_cleanup(
			write_message_X(Size,Stream,Op,Ticket,InStream),
			close(InStream)
		)
    ).
write_message_X(Size,Stream,Op,Ticket,InStream):-
    (	Size==0
    ->	Flags0=[]
    ;	byte_count(InStream,0)
    ->	Flags0=[]
    ;	Flags0=[continuation]
    ),        
    (	Size > 0xFFFF
    ->	BodyLen is 0xFFFF,
    	Flags=[incomplete|Flags0],
    	Remaining is Size - 0xFFFF
    ;	BodyLen=Size,
    	Flags=Flags0,
    	Remaining = 0
    ),
	make_header([flags(Flags),op(Op),ticket(Ticket),body_len(BodyLen)],Header),
	write_header(Stream,Header),
	(	BodyLen > 0
	->	copy_stream_data(InStream,Stream,BodyLen)
	;	true
	),
	(	Remaining > 0
	->	write_message_X(Remaining,Stream,Op,Ticket,InStream)
	;	true
	).




unify([]).
unify([Name=Var|VarNames]):-
    Name=Var,
    unify(VarNames).

encode_body([],[]).
encode_body(name(Name),Body):-
    atom_to_memory_file(Name,Body).
encode_body(cterm(VarNames,Term),Body):-
    new_memory_file(Body),
    open_memory_file(Body,write,Stream,[encoding(utf8)]),
    set_stream(Stream,encoding(utf8)),
    call_cleanup(
    	( 	\+ \+ (unify(VarNames),write_canonical(Stream,Term)),
    		write(Stream,'.')
    	),
    	close(Stream)
    ).
encode_body(uint(Integer),Body):-
	new_memory_file(Body),
    open_memory_file(Body,write,Stream,[encoding(octet)]),
    set_stream(Stream,encoding(octet)),
    call_cleanup(
    	write_integer_bytes(Stream,4,Integer),
    	close(Stream)
    ).
	
decode_body([],_).
decode_body(cterm(VarNames,Term),Body):-
    open_memory_file(Body,read,Stream,[encoding(utf8)]),
	call_cleanup(
		read_term(Stream,Term,[variable_names(VarNames)]),
		close(Stream)
	).
decode_body(name(Name),Body):-
    memoryfile_to_atom(Body,Name).
decode_body(uint(Integer),Body):-	
    open_memory_file(Body,read,Stream,[encoding(octet)]),
    set_stream(Stream,encoding(octet)),
    call_cleanup(
    	read_bytes_to_integer(Stream,4,Integer),
    	close(Stream)
    ).
    

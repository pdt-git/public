:- module(pifcom_codec,
	[	pifcom_read_message/4,
		pifcom_write_message/4,
		pifcom_read_and_decode_message/4,
		pifcom_encode_and_write_message/4,		
		pifcom_encode_body/3,
		pifcom_decode_body/3
	]
).
:- dynamic opc/3, flc/2, cleanup/1.
:- use_module(library(record)).
:- use_module(library(memfile)).

:- forall(cleanup(Ref),erase(Ref)).
:- retractall(cleanup(_)).

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
    	(	opc(OpCode,_,Type),
    		pifcom_decode_body(Type,Body,Data)
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
    opc(OpCode,_,Type),
    pifcom_encode_body(Type,Data,Body),
    call_cleanup(
    	pifcom_write_message(Stream,OpCode,Ticket,Body),
    	free_memory_file(Body)
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
spyme.

close_memfile(Out,MF):-
	spyme,
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



%% pifcom_encode_body(+Type,+Data,-Body).
% encode data to a pifcom message body.
% 
% Currently supported data formats are

% 	* cterm_image - Data is interpreted as a single term which is written 
%	  				using write_canonical/2. The term is ALWAYS followed by a full-stop.
%					Any data following the full-stop should be ignored.
%	* tickets - Data is interpreted as a list of ticket identifiers. The list is encoded
% 				as a sequence of 2-byte unsigned integers.
%	* subst_header - Data is interpreted as a list of variable numbers.
%					 The list is encoded as a sequence of single-byte unsigned integers.
%					
%   * protocol_error - expects Data to be of the form error(code,tickets), where code is an 
%					   error code ande tickets is a list of ticket identifiers.
%					 The information is encoded using a single byte unsigned integer to represent the 
%					 error code followed by a sequence of 2-byte unsigned integers to represent the ticket numbers.
%
% @param Type the body format to encode to.
% @param Data the data to encode
% @param Body a memfile containing the encoded body.
pifcom_encode_body(Type,Data,Body):-
    new_memory_file(Body),
    open_memory_file(Body,write,Stream,[encoding(octet)]),
    set_stream(Stream,encoding(octet)),
    call_cleanup(
    	encode_body(Type,Data,Stream),
    	close(Stream)
    ).
    	

%% pifcom_decode_body(+Type,+Body,-Data).
% decode a pifcom message body.
% 
% See pifcom_encode_body/3 for supported encodings.
%
% @param Type the body format to encode to.
% @param Body a memfile containing the encoded body.
% @param Data the decoded data term.
pifcom_decode_body(Type,Body,Data):-
	open_memory_file(Body,read,Stream,[encoding(octet)]),
    set_stream(Stream,encoding(octet)),
    call_cleanup(
    	decode_body(Type,Stream,Data),
    	close(Stream)
    ).


opc(0x0, mark, []).
opc(0x1, skip, []).
opc(0x2, cut, []).
opc(0x3, abort, []).
opc(0x4, complete, []).
opc(0x5, timeout, []).
opc(0x6, fail, []).
opc(0x7, bye, []).
opc(0x8, query, term_image).
opc(0x9, begin_names, subst_header).
opc(0xA, begin_solution, subst_header).
opc(0xB, error, term_image).
opc(0xC, multi_complete, tickets).
opc(0xD, binding, term_image).
opc(0xE, [], []).
opc(0xF, protocol_error, protocol_error).

:- forall(opc(A,B,C),
	(	assert(opc(B,A,C),Ref),
		assert(cleanup(Ref))
	)
).

type_opc(Type,OpC):-
    OpC is Type /\ 0xF.

type_op(Type,Op,Body):-
	type_opc(Type,OpC),
	opc(OpC,Op,Body).

flc(0x08,body).
flc(0x10,reserved1).
flc(0x20,reserved2).
flc(0x40,incomplete).
flc(0x80,continuation).

:- forall(flc(A,B),
	(	Head=..[B,Type],
		assert((Head:-A is A /\ Type),Ref),
		assert(cleanup(Ref))
	)
).

:- forall(flc(A,B),
		(	assert(flc(B,A),Ref),
			assert(cleanup(Ref))
		)
	).

:- record header(flags=[],op=0,ticket=0,body_len=0).



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
	spyme,
	read_message_continuation(NextFlags,Stream,OutStream).
read_message_continuation(_,_,_).

write_message(Stream,Op,Ticket,MemFile):-
    size_memory_file(MemFile,Size),
    open_memory_file(MemFile,read,InStream,[encoding(octet)]),
    set_stream(InStream,encoding(octet)),    
    call_cleanup(
		write_message_X(Size,Stream,Op,Ticket,InStream),
		close(InStream)
	).
write_message_X(0,_Stream,_Op,_Ticket,_InStream):-
    !.
write_message_X(Size,Stream,Op,Ticket,InStream):-
    (	byte_count(InStream,0)
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
	copy_stream_data(InStream,Stream,BodyLen),
	write_message_X(Remaining,Stream,Op,Ticket,InStream).


encode_tickets([],_Stream).
encode_tickets([T|Ts],Stream):-
    write_ticket(Stream,T),
    encode_tickets(Ts,Stream).

encode_vnums([],_Stream).
encode_vnums([N|Ns],Stream):-
    write_vnum(Stream,N),
    encode_vnums(Ns,Stream).

write_vnum(Stream,N):-
    put_byte(Stream,N).

encode_body(cterm_image,Data,Stream):-
    write_canonical(Stream,Data),
    write(Stream,'.').
encode_body(tickets,Data,Stream):-	
	encode_tickets(Data,Stream).
encode_body(subst_header,Data,Stream):-	
	encode_vnums(Data,Stream).	
encode_body(protocol_error,error(Code,Tickets),Stream):-	
	put_byte(Stream,Code),
	encode_tickets(Tickets,Stream).		


decode_tickets(Stream,[]):-
	at_end_of_stream(Stream),
 	!.  
decode_tickets(Stream,[T|Ts]):-
	read_ticket(Stream,T),
	decode_tickets(Stream,Ts).
decode_vnums(Stream,[]):-
    at_end_of_stream(Stream),
 	!.  
decode_vnums(Stream,[N|Ns]):-
    read_byte(N),
    decode_vnums(Stream,Ns).
	
decode_body(cterm_image,Stream,Term):-
	read(Stream,Term).
decode_body(tickets,Stream,Tickets):-
	decode_tickets(Stream,Tickets).
decode_body(subst_header,Stream,VNums):-
	decode_vnums(Stream,VNums).	
decode_body(protocol_error,Stream,error(Code,Tickets)):-
    get_byte(Stream,Code),
    decode_tickets(Stream,Tickets).
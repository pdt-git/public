%:- module(pifcom_echo,[pifcom_echo/0, pifcom_echo_raw/0]).
:- use_module(library(memfile)).
:- use_module(library(socket)).
:- use_module(pifcom_codec).


boing.

%%
% pifcom_echo.
%
% Tries to read and decode a pifcom message from the current input.
% re-encodes it and sends it back on the current output.
% calls halt/0 when done.
pifcom_echo:-
    set_stream(current_input,encoding(octet)),
    set_stream(current_input,tty(false)),
    set_stream(current_output,encoding(octet)),
    set_stream(current_output,tty(false)),    
	call_cleanup(
		(	pifcom_read_and_decode_message(current_input,OpC,Ticket,Data),
			pifcom_encode_and_write_message(current_output,OpC,Ticket,Data)
		),
		halt
	).

%%
% pifcom_echo_raw.
%
% Same as pifcom_echo/0, but does not try to decode/encode the message body.
	
pifcom_echo_raw:-
    set_stream(current_input,encoding(octet)),
    set_stream(current_input,tty(false)),
    set_stream(current_output,encoding(octet)),
    set_stream(current_output,tty(false)),    
    call_cleanup(
	   	pifcom_echo_raw(current_input,current_output),
   		halt
   	).
    

	
    

pifcom_echo_raw(Port):-
    tcp_socket(ServerSocket),    
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	udp_socket(USocket),
	udp_send(USocket, "listining", localhost:Port, []),
	tcp_accept(ServerSocket,Slave,_),

   	tcp_open_socket(Slave, In, Out),
   	set_stream(In,encoding(octet)),
    set_stream(In,tty(false)),
    set_stream(Out,encoding(octet)),
    set_stream(Out,tty(false)),    
   	call_cleanup(
   		pifcom_echo_raw(In,Out),
   		(	close(In),
   			close(Out),
   			halt
   		)
   	).
   	
    
pifcom_echo_raw(In,Out):-    
	call_cleanup(
		(	pifcom_read_message(In,OpC,Ticket,Body),
			pifcom_write_message(Out,OpC,Ticket,Body)
		),
		free_memory_file(Body)		
	).

test_file:-
    new_memory_file(MF),
    open_memory_file(MF,write,MOut),    
    put_byte(MOut,0x00),
    put_byte(MOut,0x7F),    
    put_byte(MOut,0x80),        
    put_byte(MOut,0xFF),    
	close(MOut),    
    open('/tmp/doofpl.raw',write,Out),
    set_stream(Out,encoding(octet)),
	pifcom_write_message(Out,binding,42,MF),
	close(Out),
	free_memory_file(MF),
	open('/tmp/doofpl.raw',read,In),
    set_stream(In,encoding(octet)),
	pifcom_read_message(In,Opc,Ticket,Body),
	close(In),
	Opc==binding,
	Ticket==42,
	open_memory_file(Body,read,MIn),
    set_stream(MIn,encoding(octet)),
    get_byte(MIn,0x00),
    get_byte(MIn,0x7F),    
    get_byte(MIn,0x80),        
    get_byte(MIn,0xFF),    
    close(MIn),
    free_memory_file(Body).

test_bytes:-
    new_memory_file(MF),
    open_memory_file(MF,write,MOut),
    put_byte(MOut,0x00),
    put_byte(MOut,0x7F),    
    put_byte(MOut,0x80),        
    put_byte(MOut,0xFF),    
	close(MOut),    
	open_memory_file(MF,read,MIn),
    open('/tmp/doofpl.raw',write,Out),
	set_stream(MIn,encoding(octet)),	
	set_stream(Out,encoding(octet)),
    copy_stream_data(MIn,Out),
    close(Out),
    close(MIn),
    free_memory_file(MF).
    
    



test_size(Bytes, Size):-
	new_memory_file(MF),
	open_memory_file(MF,write,MFOut),
	set_stream(MFOut,encoding(octet)),
	put_byte(MFOut,192),
	put_byte(MFOut,128),
	close(MFOut),
	size_memory_file(MF,Size),	
	open_memory_file(MF,read,MFIn),
	set_stream(MFIn,encoding(octet)),
	read_bytes(MFIn,Bytes),
	close(MFIn),
	free_memory_file(MF).


read_bytes(Stream,[]):-
    at_end_of_stream(Stream),
    !.
read_bytes(Stream,[B|Bs]):-
    get_byte(Stream,B),
    read_bytes(Stream,Bs).
    


gen_data:-
    retractall(my_byte(_,_)),
	flag(count,_,0),
    repeat,
	    flag(count,C,C+1),
	    R is random(256),
		assert(my_byte(C,R)),
		C == 255,
    !.

put_data(Out,K,L):-
    
	flag(count,_,K),
    repeat,
	    flag(count,C,C+1),
	    my_byte(C,R),
	    format("byte: ~16r, dec: ~d, char: '~c' ~n",[R,R,R]),
	    put_byte(Out,R),
		C == L,
    !.

    
test_code(Byte):-
	new_memory_file(MF),
	open_memory_file(MF,write,Out),
	set_stream(Out,encoding(octet)),
	put_byte(Out,0x20),
	put_byte(Out,Byte),
	put_byte(Out,0x20),
	byte_count(Out,Pos),
	close(Out),
	size_memory_file(MF,Size),
	free_memory_file(MF),
	Pos==Size.


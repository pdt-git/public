
:- dynamic my_socket/1.

otp(Port):-
	open_terminal_port(Port).

open_terminal_port(Port) :-
  redirect_to_port(Port).
  
redirect_to_port(Port) :-
  tcp_socket(Socket),
  assert(my_socket(Socket)),
  tcp_bind(Socket, Port),
  tcp_listen(Socket, 5),
	  catch(accept_terminal(Socket), E, print_message(error, E)),  
	  terminal_run_interactor.
 
accept_terminal(Socket) :-
  tcp_accept(Socket, Slave, _),
  tcp_open_socket(Slave, In, Out),  
  thread_create(terminal_attach(In,Out),_,[detached(true)]).
  
terminal_attach(In,Out) :-
  thread_self(Id),  
  assert(has_console(Id, In, Out, Out)),
  set_stream(In,  alias(user_input)),
  set_stream(Out, alias(user_output)),
  set_stream(Out, alias(user_error)),
  set_stream(In,  alias(current_input)),
  set_stream(In, buffer(false)),
  set_stream(Out, alias(current_output)),
  set_stream(Out, buffer(false)),
  set_stream(Out, alias(current_error)),
  
%  thread_create(read_in(In),_,[detached(true)]),
  format("hello peer!~n",[]),    
  thread_at_exit(terminal_detach(Id)),
  terminal_run_interactor.
  
read_in(In) :-
  get_char(In, C),
  print(C),
  read_in(In).

terminal_detach(Id):-
  format("good bye peer!~n",[]),    
  retract(has_console(Id, In, Out, Out)),
  catch(close(In), _, true),
  catch(close(Out), _, true),
  my_socket(Socket),
  retractall(my_socket(_)),
  tcp_close_socket(Socket).

terminal_run_interactor :-
  terminal_run_prolog.

terminal_run_prolog :-
  catch(prolog, E,( print_message(error, E),
  terminal_run_prolog)).
  
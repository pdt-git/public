:- load_foreign_library('/home/lukas/eclipse/workspace/PrologInterface/c/ttymode').
:- dynamic test_input/1.

%% a test.
test_ttymode(TestThread):-
    current_input(Stream),
    assert(test_input(Stream)),
    thread_create(test_loop,TestThread,[detatched(true),alias(test_thread)]),
    get_single_char(_).
    
    

test_loop :-
    test_input(Stream),
    retractall(test_input(Stream)),
    repeat,
    ttymode(Stream,Mode),
    ( 	Mode==no_tty
    ->fail
	;	format("jipee! Mode is ~a~n",[Mode]),true).
    
    
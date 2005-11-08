% Author: Tobias
% Date: 06.02.2003

:- dynamic errorhandling/0.
:- dynamic halt_on_error/0.

% temporary necessary while aspects are not 
% completely represented in prolog factbase:
:- dynamic slAspectT/4. 
:- multifile slAspectT/4.

/**
 * t_i_based_error_handling(Msg)
 * 
 */
t_i_based_error_handling(Msg) :-
    throw(term_info_exception(add_error_term_based_on_t_i, [Msg])).


add_error_term_based_on_t_i(AbbaId, Msg) :-
    abba:node(AbbaId,_,_),
    abba:property(AbbaId,position(Start,Length)),
    abba:get_defining_file(AbbaId,File),
	error_handling_add_error_term(sourceLocation(File,Start,Length),fail, Msg).
    
/**
 * abba:get_defining_file(AbbaId,File)
 *
 * look up the defining file for the current
 * abba node.
 * If the file can not be found a stack trace
 * will be shown and a file_not_found(Msg) exception thrown.
 */

abba:get_defining_file(AbbaId,File) :-
    abba:property(AbbaId,file(File)),
    !.

abba:get_defining_file(AbbaId,File) :-
    abba:edge(_,parent,_Label,AbbaId,Parent),
	abba:get_defining_file(Parent,File),
	!.
	
abba:get_defining_file(AbbaId,_File) :-
	prolog_current_frame(Frame),
	stack_for_frame_atom(Frame,Stack),
	write(Stack),
	sformat(Msg,'could not find the defining abba file for the id: ~w',[AbbaId]),
    throw(file_not_found(Msg)).

/**
 * error_handling_add_error_term(+SourceLocation, +Goal, +Message)
 *
 * SourceLocation: sourceLocation(+File, +Start, +Length)
 *                 use the predicate tree_source_location/2 
 *                 to retrieve the source location.
 */

error_handling_add_error_term(_,_term, _) :-
    call(_term),
    !.
error_handling_add_error_term(SourceLocation,_, Err) :-
    %term_to_atom(_term,_err),
    sformat(ErrorString, 'err ~w',[Err]),
    write(ErrorString),
    addErrorFacts(SourceLocation,Err),  
    flush_output,
    debugme,
    (
      halt_on_error ->
	    halt;
	    throw(Err)
	 ).
       
error_handling(_term, _,_) :-
    call(_term),
    !.
error_handling(_,_format,_term) :-
    term_to_atom(_term,_atom),
    sformat(_err,_format,[_atom]),
    error_handling_add_error_term(null,fail,_err).


/**
 * tree_source_location(+Tree,?sourceLocation(Path, Begin,Length))
 *
 * Bind sourceLocation/3 fact for tree.
 */

tree_source_location(Tree,sourceLocation(Path, Begin,Length)):-
	slT(Tree,Begin,Length), 
	getToplevel(Tree,TL),
	toplevelT(TL,_,Path,_).


addErrorFacts(null,_).
addErrorFacts(sourceLocation(File, Start, Length),Err):-
   new_id(ID),
   add(isErrorWarningMessage('declare error', ID, Err)), 
   add(slAspectT(ID, File,Start, Length)).


/**
 * Public API
 *
 * stack_for_frame_atom(+Frame,-StackTraceAtom)
 *
 * will bind StackTraceAtom to an linefeed separated list
 * of stack trace elements.
 *
 * use prolog_current_frame(Frame) 
 * to retrieve current frame.
 */

stack_for_frame_atom(Frame,StackTrace):-
    stack_for_frame(Frame,List),
    list_to_line_sep_string(List, StackTrace).

/**
 * Public API
 *
 * stack_for_frame(+Frame,-StackTraceList)
 *
 * use prolog_current_frame(Frame) to retrieve current frame.
 */
   
stack_for_frame(Frame,[]):-
    prolog_frame_attribute(Frame,top,true),
    !.
    
stack_for_frame(Frame,[Info|Stack]) :-
    frame_info(Frame,Info),
    prolog_frame_attribute(Frame,parent,Parent),
	stack_for_frame(Parent,Stack).

stack_for_frame(_Frame,['stack inspection failed']):-
    !.

    
frame_info(Frame,Info):-
    prolog_frame_attribute(Frame,clause,Ref),
    prolog_frame_attribute(Frame,level,Level),
    prolog_frame_attribute(Frame,goal,Goal),
    term_to_atom(Goal, GoalAtom),
%    nth_clause(Pred,_,Ref),
    clause_property(Ref,file(File)),
    clause_property(Ref,line_count(Line)),
%    functor(Pred,Name,Arity),
%    term_to_atom(Pred,Atom),
%    write(Atom),
    sformat(Info,'~a:~a ~a level: ~a~n',[File,Line,GoalAtom,Level]).
       
       
list_to_line_sep_string([],'').

list_to_line_sep_string([Head|Tail],String):-
	list_to_line_sep_string(Tail,StringTail),
	sformat(String,'~a~n~a',[Head,StringTail]).
	
	
/********* Currently still unused (speculative generality): ************ */
	
/**
 * throw_exception_upon_failure(+Pred,+Msg) 
 *   Call Pred. If it fails print Msg and a stack trace.
 *   Then throw Pred as an exception. 
 */
throw_exception_upon_failure(Pred,_Msg):-
       call(Pred),               
       !.
throw_exception_upon_failure(Pred,Msg):-
     prolog_current_frame(Frame),
     stack_for_frame_atom(Frame,Trace),
     sformat(Msg2, 'Failed predicate: ~w~n~w',[Pred,Trace]),
     concat(Msg,Msg2,ExtMsg),
     write(ExtMsg),
        % ExceptionTerm =..[ExceptionType,ExtMsg],
     throw(Pred/*ExceptionTerm*/).
       
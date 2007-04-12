:- module(treefactwriter,[
writeTreeFacts/1,
writeTreeFacts/2,
clearPersistantFacts/0,
clearTreeFactbase/1,
clearTreeFactbase/0
]).

/**
 * treefactwriter writes all current tree elements 
 * to a specified pl file.
 * 
 * Schmatz, 2006-10-10: << API update #1 >>
 * 
 * You can call 'writeTreeFacts' with a second
 * parameter which has no effect while not equal
 * to the string 'javalangfactsmode'. When equal
 * to 'javalangfactsmode' additional
 * permanent_java_lang_class/1 facts are written
 * which indicate the all written class PEFs as
 * 'java.lang' classes. With this I can ensure
 * that such classes are not be retracted within
 * Ditrios.
 * ---------- END : << API update #1 >>
 *
 */

writeTreeFacts(File) :-
	Mode = 'defaultmode',
	internal_writeTreeFacts(File, Mode).

% For Ditrios (Schmatz)
writeTreeFacts(File, Mode) :-
	internal_writeTreeFacts(File, Mode).
% END - For Ditrios (Schmatz)

internal_writeTreeFacts(File, Mode) :-
    open(File, write, Stream,[]),
    set_stream(Stream, encoding(utf8)),
	write(Stream,'encoding(utf8).'),
	nl(Stream),
	% For Ditrios (Schmatz)
	forall(
		(
			Mode = 'javalangfactsmode',
			Fact = permanent_java_lang_class(_Class),
			call(Fact),
			term_to_atom(Fact, Atom)
		),	
		catch(
			format(Stream, '~w.~n', Atom),
			Exception,
			debugme)
	),
	% END - For Ditrios (Schmatz)

    forall((persistant(Fact), %Fact = sourceLocation(_,_,_,_), 
    		 call(Fact),
    		 term_to_atom(Fact, Atom)
    	    ),
    	    catch(
    	    
    		format(Stream, '~w.~n',Atom),
    				Exception,
				debugme(Atom))
    		
    ),
    		
    forall(globalIds(FQN,Id),
    	   format(Stream, 'globalIds(''~w'',~w).~n',[FQN,Id])
    ),
    
    lastID(LastID),
    format(Stream, ':- retractall(lastID(_)),assert(lastID(~w)).',[LastID]),
    close(Stream).

/**
 * persistant(-Fact)
 * All facts tree facts, 
 * 
 */    
    
persistant(Fact) :-
    treeFact(Fact).   

%persistant(Fact) :-         
%	(	(Head=ct,Arity=3)
%	;	(Head=aj_ct_list,Arity=1)
%	),
%	uniqueArgumentList(Arity,Arguments),
%	Fact =.. [Head|Arguments].
     
/*
 treeFact(-Fact)
*/    
treeFact(Fact) :-
    (
      treeSignature(Head,Arity);
      attribSignature(Head,Arity)
    ),
    uniqueArgumentList(Arity,Arguments),
    Fact =.. [Head|Arguments].

clearTreeFactbase :-
    rollback,
    clearPersistantFacts.
%    catch( 
%	clearTreeFactbase(_Project),
%	Exception,
%	(write(Exception), write(' Deleting all PEFs. '),clearPersistantFacts)).
	

clearTreeFactbase(Project) :-
    rollback,
%   forall((treeFact(A),user:call(A)),user:retract(A)).
	forall((toplevelT(TL,_,_,_),projectLocationT(TL,Project,_)),
	        delete_toplevel(TL)).

clearPersistantFacts :-
   persistant(A),
   call(A),
   retract(A),
   fail.
   
clearPersistantFacts.
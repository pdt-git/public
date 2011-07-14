/** <module> Extension mechanism similar to extension points in Eclipse

Arbitrary modules may contribute to an extension point and are evaluated one after another
without backtracking. If an extension point does not succeed an error handler predicate is called 
with the handler(ExtModule:Goal,Arg1,...).  

To use this extension import the extension_call/3 predicate:

 :- use_module(extension(extension),[extension_call/3]).

<h2>Define and call an extension point</h2>

The defining module does not need to declare an extension point explicitly.
It just calls:

     extension_call(<extension point name>,<argument list>,<error handler predicate>)

The second parameter must be a list, the handler must have the arity 1.  

<h3>Example</h3>

Call the extension check_ct:

 ... extension_call(check_ct,[CT],error_msg_on_fail) ...
 
error_msg_on_fail(ErrorTerm):- ....

<h2>Contribute to an extension point</h2>

The file contributing to an must be a module and it defines the extension point via the following pattern:

<target_module>:extension(<extension point name>,<own module>,<goal to call on execution>).

<h3>Example</h3>

ct_checker extends its on extension point:
  ct_checker:extension(check_ct,ct_checker,check_ct).
laj_lpane contributes to the extension point:
  ct_checker:extension(check_ct,laj_lpane, laj_type_inference).
 	
@author Tobias Rho
@license EPL
*/

:- module(extension_point,
		[
		extension_call/3 % extension_call(+ExtensionTerm,ErrorHandler) 
		]).

:- module_transparent extension_call/3. % extension_call(+ExtensionTerm, 
		
		
extension_call(ExtensionTerm,Args,ErrorHandler):-
    context_module(ContextModule),
    forall(
    	ContextModule:extension(ExtensionTerm,ExtModule,GoalFunctor),
    	(
    		Goal=..[GoalFunctor|Args],
    		ErrorHandlerGoal=..[ErrorHandler,ExtModule:Goal|Args],
    		extension_point:error_handling(ExtModule:Goal,ContextModule:ErrorHandlerGoal)
    	)).
    
error_handling(ExtModule:Goal,_ErrorHandler):-
    once(ExtModule:Goal),
    !.

error_handling(_,ErrorHandler):-
	call(ErrorHandler).
    
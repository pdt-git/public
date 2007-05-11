:-module(processor,[
	
]).


execute_action(nop,_).
execute_action(request(Target),P):-
    
    
    

/*
a job is a tuple 
job(target,wait_for)
where the first arguemtn is the target the job wants to build
the second argument is either nil or the target the job is currently waiting for.

The state of a processor is a tuple
processor_state(activity, stack)
activity: idle, running or waiting
stack: a list of jobs beeing processed by this processor. Head == Top.
*/
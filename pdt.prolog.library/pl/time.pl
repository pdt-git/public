

:- module_transparent time/2.    
  
time(Goal0,time(UsedInf, UsedTime, Wall, Lips)) :-
	expand_goal(Goal0, Goal),
	get_time(OldWall),
	statistics(cputime, OldTime), 
	statistics(inferences, OldInferences), 
	(   catch(Goal, E, true)
	->  Result = yes
	;   Result = no
	),
	statistics(inferences, NewInferences), 
	statistics(cputime, NewTime), 
	get_time(NewWall),
	UsedTime is NewTime - OldTime, 
	UsedInf  is NewInferences - OldInferences - 3, 
	Wall     is NewWall - OldWall,
	(   UsedTime =:= 0
	->  Lips = 'Infinite'
	;   Lips is integer(UsedInf / UsedTime)
	),
	%TR: removed: 
	%print_message(informational, time(UsedInf, UsedTime, Wall, Lips)), 
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).
	

demo_time_measurement :-
   startStopwatch, 
   allclasses(_NR),
   reportRuntime('Counting of all classes took ').  

/**
 * Helper predicates for reporting time spent.
 *   - performance(Goal, Time, Count)
 *   - ctc_time(Goal, Time)
 *   - startStopwatch
 *   - reportRuntime(ForWhat)
 *   - reportRuntime(ForWhat,CPUMilisSinceLast)
 */
   
      
/**
 * Measure milliseconds to find and count all results of a Goal.
 */ 
performance(Goal, Time, CountAll) :- 
  ctc_time(count(Goal, CountAll), Time).

/**
 * Measure time to find and count all results of a Goal and
 * and also all unique results. 
 */   
performanceUnique(Goal, Time, CountAll,CountUnique) :- 
  ctc_time(count_all_and_unique(Goal,CountAll,CountUnique), Time).



ctc_time(Call, Time) :- 
   startStopwatch, 
     call(Call),
   measureRuntime(Time).
    

startStopwatch :-
    statistics(runtime, _CPU),    % Start new CPU timer
    statistics(real_time, _Real). % Start new real timer
 
 
measureRuntime(CPUMilisSinceLast) :- 
    statistics(runtime,    [_CPUMilisSinceStart, CPUMilisSinceLast]). 

    
reportRuntime(ForWhat) :- 
    statistics(runtime,    [_CPUMilisSinceStart, CPUMilisSinceLast]),   
    statistics(real_time,  [_RealSecsSinceStart, RealSecsSinceLast]), 
    log_on_stdout('~a: CPU = ~a milliseconds, real time ca. ~a seconds~n', 
           [ForWhat,CPUMilisSinceLast,RealSecsSinceLast]).
           
    
reportRuntime(ForWhat,CPUMilisSinceLast) :- 
    statistics(runtime,    [_CPUMilisSinceStart, CPUMilisSinceLast]),   
    statistics(real_time,  [_RealSecsSinceStart, RealSecsSinceLast]), 
    log_on_stdout('~a: CPU = ~a milliseconds, real time ca. ~a seconds~n', 
           [ForWhat,CPUMilisSinceLast,RealSecsSinceLast]).    


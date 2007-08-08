:- use_module(library('builder/builder')).

nono(No).

:- debug(race).
pdt_builder:build_hook(raceA):-
    pdt_request_target(race1),
    pdt_request_target(race2).
pdt_builder:build_hook(raceB):-
    pdt_with_targets([race1],true),
    pdt_invalidate_target(race1),
    pdt_request_target(race1),
        pdt_request_target(race2).

pdt_builder:build_hook(race1):-
    sleep(5).
pdt_builder:build_hook(race2):-
    sleep(5).




pdt_builder:invalidate_hook(race1):-
    pdt_invalidate_target(raceA),
    pdt_invalidate_target(raceB).
pdt_builder:invalidate_hook(race2):-
    pdt_invalidate_target(raceA),
    pdt_invalidate_target(raceB).

run(T):-

    debug(race,"invalidating~n~n",[]),

        pdt_invalidate_target(race1),
        pdt_invalidate_target(race2),
        debug(race,"requesting~n~n",[]),
        pdt_with_targets([T],
                (       debug(race,"have all locks. sleeping.~n~n",[]),
                        sleep(5.0),
                        debug(race,"done. releasing locks.~n~n",[])
                )
        ).

race:-
    thread_create(run(raceA),_,[alias(racer_A)]),
    thread_create(run(raceB),_,[alias(racer_B)]),
    thread_join(racer_A,RA),
    thread_join(racer_B,RB),
    debug(race,"racer_A result: ~w~n",[RA]),
    debug(race,"racer_B result: ~w~n",[RB]).

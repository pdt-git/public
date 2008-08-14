:- use_module(builder).
pdt_builder:build_hook(t1):-
    catch(pdt_request_target(t2),error(cycle(_)),true).
pdt_builder:build_hook(t2):-
    catch(pdt_request_target(t1
    ),error(cycle(_)),true).
    
pdt_builder:target_mutable(t1,true).
pdt_builder:target_mutable(t2,true).

do_test:-
    pdt_invalidate_target(t1),
    pdt_invalidate_target(t2),
    pdt_request_target(t1).
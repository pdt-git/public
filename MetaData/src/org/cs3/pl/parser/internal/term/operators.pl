infix_precs(Op,Prec,Prec-1,Prec-1):-
    current_op(Prec,xfx,Op).
infix_precs(Op,Prec,Prec,Prec-1):-
    current_op(Prec,yfx,Op).
infix_precs(Op,Prec,Prec-1,Prec):-
    current_op(Prec,xfy,Op).
infix_precs(Op,Prec,Prec,Prec):-
    current_op(Prec,yfy,Op).
    
prefix_precs(Op,Prec,Prec):-
    current_op(Prec,fy,Op).
prefix_precs(Op,Prec,Prec-1):-
    current_op(Prec,fx,Op).    

gen_infix_maps:-
    forall(
      infix_precs(Op,Prec,Lh,Rh),
      (
      	escape(Op,Esc),
	    format("		infix_map.put(\"~w\",new int[]{~w,~w,~w});~n",[Esc,Prec,Lh,Rh])
	   )
    ).

gen_prefix_maps:-
    forall(
      prefix_precs(Op,Prec,Rh),
      (
      escape(Op,Esc),
      format("		prefix_map.put(\"~w\",new int[]{~w,~w});~n",[Esc,Prec,Rh])
      )
    ).
    
gen_prefix_tks:-
	forall(
      prefix_precs(Op,_,_),
      (
      escape(Op,Esc),
      format("		|\"~w\"~n",[Esc])
      )
    ).

gen_infix_tks:-
	forall(
      infix_precs(Op,_,_,_),
      (
      escape(Op,Esc),
      format("		|\"~w\"~n",[Esc])
      )
    ).

gen_op_tks:-
	forall(
      current_op(_,_,Op),
      (
      escape(Op,Esc),
      format("		|\"~w\"~n",[Esc])
      )
    ).    
escape(In,Out):-
  atom_chars(In,InList),
  escape_l(InList,OutList),
  atom_chars(Out,OutList).

escape_l([],[]).
escape_l( ['\\'|ITail], ['\\','\\'|OTail]):-
    !,
  escape_l(ITail,OTail).
escape_l([A|ITail],[A|OTail]):-
    escape_l(ITail,OTail).
gen_maps:-
	gen_infix_maps,
	gen_prefix_maps.    
	
allops:-
  forall(infix_precs(Op,_,_,_);prefix_precs(Op,_,_),(write_canonical(Op),nl)).
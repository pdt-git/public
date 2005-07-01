:- module( test_composition_vars_pp_pn, []).

test(compose(vars_pp_pn) ) :- 
     ct_compose( and_seq([replaceReadAccesses(C,F,T,G),
                          changeFieldAccessToPrivate(C,F)
                        ]),
                 Composite),
     ct( compositeOf4and5(C,F,T,G), Pre, Post ),
     assert_true('Wrong composition result: ', 
                 Composite = ct(_, Pre, Post)
                 ).

/* INPUT: ------------------------------------------------------- */

ct(replaceReadAccesses(C,F,T,G), (
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,T,N,_),
    % Getter method "T G()" exists:
    methodDefT(M,C,G,[],T,_,_),
    % There is a access to the field F:
    getFieldT(GF,Par,Enc,Rcv,N,F),
    % ... outside of the getter method:
    Enc \= M
),(
    replace(getFieldT(GF,Par,Enc,Rcv,N,F),
            applyT(A,Par,Enc,Rcv,G,[],M)
    )
  )
).

ct(changeFieldAccessToPrivate(C,F), (
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,T,N,_),
    modifier(F,Modif),
    ( Modif = private
    ; Modif = protected
    ; Modif = public
    ; Modif = package
    )
),(
    replace(modifier(F,Modif),
            modifier(F,private)
    )
  )
).

/* -------------------------------------------------------------------- */

/* -------------------------------------------------------------
Komposition von
   CT4= replaceReadAccesses(C,F,T,G)
   CT5= changeFieldAccessToPrivate(C,F)
liefert:
   C45= C4 und Contrib(T4(C5))
   T45= T4,T5
da Postcond(T4) =
        not(getFieldT(GF,Par,Enc,Rcv,N,F)),
        applyT(A,Par,Enc,Rcv,G,[],M)
disjunkt zu C5 ist, ändert sich an C5 nichts,
d.h. Contrib(T4(C5)) = C5. Insgesamt also:
*/

ct( compositeOf4and5(C,F,T,G), (
    %------------- C4:
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,T,N,_),
    % Getter method "T G()" exists:
    methodDefT(M,C,G,[],T,_,_),
    % There is a access to the field F:
    getFieldT(GF,Par,Enc,Rcv,N,F),
    % ... outside of the getter method:
    Enc \= M,
    %-------------- Contrib(T4(C5)) = C5
    classDefT(C,_,_,_),not(externT(C)),   % <--- redundant zu C4
    fieldDefT(F,C,T,N,_),                 % <--- redundant zu C4
    modifier(F,Modif),
    ( Modif = private
    ; Modif = protected
    ; Modif = public
    ; Modif = package
    )
   ,
   newId(A)
),(
    replace(getFieldT(GF,Par,Enc,Rcv,N,F),
            applyT(A,Par,Enc,Rcv,G,[],M)
    ),
    replace(modifier(F,Modif),
            modifier(F,private) )
  )
).

/*
Das redundanzfreie Zwischenergebnis ist:
*/
ct( compositeOf4and5noRedundancies(C,F,T,G), (
    %------------- C4:
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,T,N,_),
    % Getter method "T G()" exists:
    methodDefT(M,C,G,[],T,_,_),
    % There is a access to the field F:
    getFieldT(GF,Par,Enc,Rcv,N,F),
    % ... outside of the getter method:
    Enc \= M,
    %-------------- Contrib(T4(C5)) = C5 minus redundante Teile
    modifier(F,Modif),
    ( Modif = private
    ; Modif = protected
    ; Modif = public
    ; Modif = package
    )
   ,
   newId(A)
),(
    replace(getFieldT(GF,Par,Enc,Rcv,N,F),
            applyT(A,Par,Enc,Rcv,G,[],M)
    ),
    replace(modifier(F,Modif),
            modifier(F,private) )
  )
).


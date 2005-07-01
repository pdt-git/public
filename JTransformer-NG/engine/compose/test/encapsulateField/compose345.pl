:- multifile ct/3.
:- dynamic ct/3.

/* -------------------------------------------------------------
Komposition von
   CT3 = replaceWriteAccesses(C,F,T,S)
   CT45= Compose( AND_SEQ( replaceWriteAccesses(C,F,T,S), 
                           changeFieldAccessToPrivate(C,F) )
liefert:
   C345= C3 und Contrib(T3(C45)) 
   T45 = T3, T45 = T3, T4, T5
da Postcond(T3) =
    applyT(A,PA,E,null,N2,[RHS],M),
    not(assignT(A,PA,E,LHS,RHS)),
    not(getFieldT(LHS,A,E,null,N,F)),
beeinflusst es die Bedingung 
    getFieldT(GF,Par,Enc,Rcv,N,F)
in C45.
disjunkt zu C5 ist, ändert sich an C5 nichts,
d.h. Contrib(T4(C5)) = C5. Insgesamt also:
*/
ct( 'C345'(C,F,T,G,S), (
    %------------- C3:
    classDefT(C,_,_,_),not(externT(C)),
        fieldDefT(F,C,T,N,_),
        % Setter method "void S(T *)" exists:
        methodDefT(M,C,S,[P],type(basic,void,0),_,_),
           paramDefT(P,M,T,_),
    % There is a direct assignment to the field F:
    assignT(A,PA,E,LHS,RHS),
        getFieldT(LHS,A,E,null,N,F),
    % ... outside of the setter method:
    E \= M,
    
    %------------- C4:
    classDefT(C,_,_,_),not(externT(C)),  % <----- redundant
    fieldDefT(F,C,T,N,_),                % <----- redundant
    % Getter method "T G()" exists:
    methodDefT(M,C,G,[],T,_,_),
    % There is a access to the field F:
    getFieldT(GF,Par,Enc,Rcv,N,F),       % <----- wird verändert
    % ... outside of the getter method:
    Enc \= M,
    %-------------- Contrib(T4(C5)) = C5 minus redundante Teile
    modifier(F,Modif),
    ( Modif = private
    ; Modif = protected
    ; Modif = public
    ; Modif = package
    )
),( % Zufällige Namensgleichheiten beseitigen:
    % T3:
    replace(assignT(A,PA,E,LHS,RHS),
            applyT(A,PA,E,null,N2,[RHS],M) ),
    delete(getFieldT(LHS,A,E,null,N,F)),
    %T4:
    replace(getFieldT(GF,Par,Enc,Rcv,N,F),
            applyT(A,Par,Enc,Rcv,G,[],M) ),
    % T5:
    replace(modifier(F,Modif),
            modifier(F,private) )
  )
).
/*
Das redundanzfreie Zwischenergebnis ist:
*/




/* INPUT zu Schritt 2 (nach compose45):  ----------------------- */


/******************************************************************
 * replaceWriteAccesses(Class,Field,Ftype,SetterName)
 */
ct(replaceWriteAccesses(C,F,T,S), (
    classDefT(C,_,_,_),not(externT(C)),
        fieldDefT(F,C,T,N,_),
        % Setter method "void S(T *)" exists:
        methodDefT(M,C,S,[P],type(basic,void,0),_,_),
           paramDefT(P,M,T,_),
    % There is a direct assignment to the field F:
    assignT(A,PA,E,LHS,RHS),
        getFieldT(LHS,A,E,null,N,F),
    % ... outside of the setter method:
    E \= M
),(
    replace(assignT(A,PA,E,LHS,RHS),
            applyT(A,PA,E,null,N2,[RHS],M)
    ),
    delete(getFieldT(LHS,A,E,null,N,F))
  )
).


ct( 'C45'(C,F,T,G), (
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
),(
    replace(getFieldT(GF,Par,Enc,Rcv,N,F),
            applyT(A,Par,Enc,Rcv,G,[],M)
    ),
    replace(modifier(F,Modif),
            modifier(F,private) )
  )
).

/* INPUT zu Schritt 1: ------------------------------------------ */

/******************************************************************
 * replaceWriteAccesses(Class,Field,Ftype,SetterName)
 */
ct(replaceWriteAccesses(C,F,T,S), (
    classDefT(C,_,_,_),not(externT(C)),
        fieldDefT(F,C,T,N,_),
        % Setter method "void S(T *)" exists:
        methodDefT(M,C,S,[P],type(basic,void,0),_,_),
           paramDefT(P,M,T,_),
    % There is a direct assignment to the field F:
    assignT(A,PA,E,LHS,RHS),
        getFieldT(LHS,A,E,null,N,F),
    % ... outside of the setter method:
    E \= M
),(
    replace(assignT(A,PA,E,LHS,RHS),
            applyT(A,PA,E,null,N2,[RHS],M)
    ),
    delete(getFieldT(LHS,A,E,null,N,F))
  )
).

/******************************************************************
 * replaceReadAccesses(Class,Field,Ftype,GetterName)
 */
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


/******************************************************************
 * changeFieldAccessToPrivate(Class,Field)
 */
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

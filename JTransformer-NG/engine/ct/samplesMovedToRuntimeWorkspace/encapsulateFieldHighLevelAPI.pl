% Author: Günter Kniesel
% Date: 20.06.2005

/**************************************************************
  encaspulateField(Class,Field,Ftype,GetterName,SetterName)
  = 'OR_SEQ'(
       addGetter(Class,Field,Ftype,GetterName),
       addSetter(Class,Field,Ftype,SetterName),
       'AND_SEQ'(
           replaceReadAccesses(Class,Field,Ftype,GetterName),
           replaceWriteAccesses(Class,Field,Ftype,SetterName),
           changeFieldAccessToPrivate(Class,Field)
       )
    )
**************************************************************/

encapsulateField(Class,Field,Ftype,GetterName,SetterName) :-
       addGetter(Class,Field,Ftype,GetterName),
       addSetter(Class,Field,Ftype,SetterName),
           replRead(Class,Field,Ftype,GetterName),
           replWrite(Class,Field,Ftype,SetterName),
           makePriv(Class,Field).

% ----- Für Test der einzelnen CTs:

addGetter(Class,Field,Ftype,GetterName) :-
   apply_ct(addGetter(Class,Field,Ftype,GetterName)).
addSetter(Class,Field,Ftype,SetterName) :-
   apply_ct(addSetter(Class,Field,Ftype,SetterName)).

replRead(Class,Field,Ftype,GetterName) :-
   apply_ct(replaceReadAccesses(Class,Field,Ftype,GetterName)).
replWrite(Class,Field,Ftype,SetterName) :-
   apply_ct(replaceWriteAccesses(Class,Field,Ftype,SetterName)).
   
makePriv(Class,Field) :-
   apply_ct(changeFieldAccessToPrivate(Class,Field)).

% ----- Alles Hintereinander als OR-Seq:

encapsFieldNonAtomic(Class,Field,Ftype,GetterName,SetterName) :-
   apply_ctlist([
     addGetter(Class,Field,Ftype,GetterName),
     addSetter(Class,Field,Ftype,SetterName),
     replaceReadAccesses(Class,Field,Ftype,GetterName),
     replaceWriteAccesses(Class,Field,Ftype,SetterName),
     changeFieldAccessToPrivate(Class,Field)
   ]).
   
 % ----- AND-Sequenz komponieren und ausführen:
 % ----- Compose ist noch nicht implementiert (text first ;)
encapsField(Class,Field,Ftype,GetterName,SetterName) :-
   compose_and_seq([
     replaceReadAccesses(Class,Field,Ftype,GetterName),
     replaceWriteAccesses(Class,Field,Ftype,SetterName),
     changeFieldAccessToPrivate(Class,Field)
     ],
     COMPOSED
   ),
   apply_ctlist([
     addGetter(Class,Field,Ftype,GetterName),
     addSetter(Class,Field,Ftype,SetterName),
     COMPOSED
   ]).
   
/**************************************************************
 * addGetter(C,F,T,G)
 *   Create a getter method "T G() { return F }" for every
 *   field F of type T in every class C, provided that
 *   a method with that signature doesn't exist yet.
 */

ct(addGetter(C,F,T,G), (
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,T,N,_),
	concat(get, N, G), 
    % No method with signature "T G()" :
    not( method(M,C,G,[],T,_,_) ),

    % Identities of elements to be created:
    new_id(M),       % new method
    new_id(B),       % its body
    new_id(R),       % its return statement
    new_id(Get)      % its getfield access
),(
    % Create Method "T G() { return F}":
    add( method(M,C,G,[],T,[],B) ),
    add( blockT(B,M,M,[R]) ),
    add( returnT(R,B,M,Get) ),
    add( getFieldT(Get,R,M,null,N,F) )
)).


/**************************************************************
 * addSetter(C,F,T,S)
 *   Create a Method "void S(T newvalue) { F = newvalue }" for every
 *   field F of of type T in every class C, provided that
 *   a method with that signature doesn't exist yet.
 */
 
ct(addSetter(C,F,T,S), (
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,T,N,_),
	concat(set, N, S), 
    % no method with signature "void S(T *)" :
    not((
    	method(M,C,S,[P],type(basic,void,0),_,_),
        paramDefT(P,M,T,_)
    )),

    % Identities of elements to be created:
    new_id(M),       % new method
    new_id(P),       % its parameter
    new_id(B),       % its body
    new_id(Exec),    % its assignment statement
    new_id(A),       % its assignment statement
    new_id(LHS),     % the left-hand-side of the assignment
    new_id(RHS)      % the lright-hand-side of the assignment
),(
    % Create Method "void S(T newvalue) { F = newvalue }":
    add( method(M,C,S,[P],type(basic,void,0),[],B) ),
    add( paramDefT(P,M,T,'newvalue') ),
    add( blockT(B,M,M,[Exec]) ),
    add( execT(Exec, B, M, A)), 
    add( assignT(A,Exec,M,LHS,RHS) ),
    add( getFieldT(LHS,A,M,null,N,F) ),
    add( identT(RHS,A,M,'newvalue',P) )
)).


/******************************************************
 * replaceWriteAccesses(Class,Field,Ftype,SetterName) *
 ******************************************************/
ct(replaceWriteAccesses(C,F,T,S), (
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,T,N,_),
    % Setter method "void S(T *)" exists:
	concat(set, N, S), 
    method(M,C,S,[P],type(basic,void,0),_,_),
    paramDefT(P,M,T,_),
    % There is a direct assignment to the field F:
    assignT(A,PA,E,LHS,RHS),
    getFieldT(LHS,A,E,Recv,N,F),
    % ... outside of the setter method:
    E \= M
),(
    replace(assignT(A,PA,E,LHS,RHS),
            applyT(A,PA,E,Recv,S,[RHS],M)
    ),
    delete(getFieldT(LHS,A,E,Recv,N,F))
  )
).


/*****************************************************
 * replaceReadAccesses(Class,Field,Ftype,GetterName) *
 *****************************************************/
ct(replaceReadAccesses(C,F,T,G), (
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,T,N,_),
    % Getter method "T G()" exists:
	concat(get, N, G), 
    method(M,C,G,[],T,_,_),
    % There is a access to the field F:
    getFieldT(GF,Par,Enc,Rcv,N,F),
    not(assignT(Par, _,_,GF,_)),
    
    % ... outside of the getter method:
    Enc \= M
),(
    replace(getFieldT(GF,Par,Enc,Rcv,N,F),
            applyT(GF,Par,Enc,Rcv,G,[],M)
    )
  )
).


/********************************************
 * changeFieldAccessToPrivate(Class,Field)  *
 ********************************************/
ct(changeFieldAccessToPrivate(C,F), (
    classDefT(C,_,_,_),not(externT(C)),
    fieldDefT(F,C,_T,_N,_),
    modifierT(F,Modif),
    ( Modif = private
    ; Modif = protected
    ; Modif = public
    ; Modif = package
    )
),(
    replace(modifierT(F,Modif),
            modifierT(F,private)
    )
  )
).

% Author: Uwe Bardey
% Date: 27.09.2003

% Implemented Low-Level CT's.
% Assumptions:
% - Readfields are represented as identT/5
% - Writefields are represented as assignT/5
%
% 1) addSetterInterf
% 2) addSetterCode
%
% 3) addCounterInterf
% 4) addCounterCode
%
% 5) addAnotherFlag
%
% 6) addPersistenceInterf
% 7) addPersistenceCode
%
% 8) addAssociationInterface
% 9) addAssociationCode
%
% Dep-Graph for setter and counter
% ?- gen_dep_graph([addSetterInterf, addSetterCode, addCounterInterf, addCounterCode]).
% Dep-Graph for storage, anotherFlag and association
% ?- gen_dep_graph([addPersistenceInterf, addPersistenceCode, addAssociationInterface, addAssociationCode, addAnotherFlag]).
% Dep-Graph for counter and storage
% ?- gen_dep_graph([addCounterInterf, addCounterCode, addPersistenceInterf, addPersistenceCode]).
% Dep-Graph for all CT's
% ?- gen_dep_graph([addSetterInterf, addSetterCode, addCounterInterf, addCounterCode, addPersistenceInterf, addPersistenceCode, addAnotherFlag, addAssociationInterface, addAssociationCode]).


% Treat before and after like AST-elements in dependency analysis
%ast_node(before,before(A,_),A).
%ast_node(after,after(A,_),A).
ast_node(before,before(A,B),before(A,B)).
ast_node(after,after(A,B),after(A,B)).
ast_edge(after,after(A,_),A).
ast_edge(after,after(_,A),A).
ast_edge(before,before(A,_),A).
ast_edge(before,before(_,A),A).


ct(addSetterInterf, (
    classDefT(C,_,_,_),not(externT(C)),
        fieldDefT(F,C,T,N,_),
        pattern('set*',[N],N2),
        not(methodDefT(M,C,N2,_,T,_,_)),

    new_id(M), new_id(P), new_id(B), new_id(R), new_id(A), new_id(LHS), new_id(RHS)
),(
    add(methodDefT(M,C,N2,[P],T,[],B)),
        add(fieldDefT(P,M,T,'newvalue',null)),
        add(blockT(B,M,M,[R])),
            add(returnT(R,B,M,A)),
                add(assignT(A,R,M,LHS,RHS)),
                    add(identT(LHS,A,M,N,F)),
                    add(identT(RHS,A,M,'newvalue',P))
)).

ct(addSetterCode, (
    classDefT(C,_,_,_),not(externT(C)),
        fieldDefT(F,C,T,N,_),
        pattern('set*',[N],N2),
        methodDefT(M,C,N2,[P],T,_,_),
        fieldDefT(P,M,T,_,_),

    assignT(A,PA,E,LHS,RHS),
        identT(LHS,A,E,N,F),

    E \= M

%    new_id(MI)
),(
    replace(assignT(A,PA,E,LHS,RHS), applyT(A,PA,E,null,N2,[RHS],M)),
        delete(identT(LHS,A,E,N,F))
%        add(identT(MI,A,E,N2,M))
)).

ct(addCounterInterf, (
    classDefT(C,_,_,_),not(externT(C)),
        fieldDefT(_,C,_,N,_),
        not(pattern('count*',[_],N)),
        pattern('count*',[N],N2),
        not(fieldDefT(_,C,type(basic,int,0),N2,_)),

    new_id(CF)
),(
    add(fieldDefT(CF,C,type(basic,int,0),N2,null))
)).

ct(addCounterCode, (
    classDefT(C,_,_,_),not(externT(C)),
        fieldDefT(F,C,_,N,_),
        not(pattern('count*',[_],N)),
        pattern('count*',[N],N2),
        fieldDefT(CF,C,type(basic,int,0),N2,_),

    assignT(A,PA,E,LHS,_),
        identT(LHS,A,E,N,F),

    % da dnf normalisierung nicht fertig, provisoritsch statt: not(a,b,c)
    % no counter incmement before assign
    not(operationT(OP, PA, E, [I], '++', 1)),
    not(identT(I,OP,E,N2,CF)),

    new_id(I), new_id(OP)
),(
    add(operationT(OP, PA, E, [I], '++', 1)),
        add(identT(I,OP,E,N2,CF)),

    add(before(A, OP))
)).



% For every class, that does not have a field called 'flag'
% add an int 'flag' field
ct(addAnotherFlag, (
    classDefT(CLASS, _, _, _),not(externT(CLASS)),
    not(fieldDefT(FLAG , CLASS, _, 'flag', _)),
    new_id(FLAG)
),(
    add(fieldDefT(FLAG , CLASS, type(basic, int, 0), 'flag', null))
)).


% For every class, that does not have a field caled 'flag'
% add a boolean 'flag' field
ct(addPersistenceInterf, (
    classDefT(CLASS, _, _, _),not(externT(CLASS)),
    not(fieldDefT(FLAG , CLASS, _, 'flag', _)),
    new_id(FLAG)
),(
    add(fieldDefT(FLAG , CLASS, type(basic, boolean, 0), 'flag', null)),
    add_to_class(CLASS,FLAG)
)).


% For every class, that has a field
% plus a boolean flag field called "flag"
% and there exists a write access to that field
% insert the following code exclusivly after each write access:
%   a = b
%   if(flag) {
%       a.flag = true
%       Storage.save(a)
%       Storage.save(this)
%   }
ct(addPersistenceCode, (
    % if there exists a storage class with a method Storage$save
    classDefT(STORE, null, 'Storage', _),
    methodDefT(SAVE, STORE, 'save', _, _, _, _),

    % and there is a field and a flag field in some class
    classDefT(CLASS, _, _, _),
    fieldDefT(FIELD, CLASS, type(class, _, _), FNAME, _),
    fieldDefT(FLAG , CLASS, type(basic, boolean, 0), 'flag', null),

    % and in this class is a write access to this field
    assignT(ASSIGN, PA, ENC, LHS, _),
       getFieldT(LHS,ASSIGN,ENC,null,FNAME,FIELD),

    % nothing else should happen after the write access - exclusively
%    not(after(ASSIGN,_)),

    new_ids([IF,IFLAG,THEN,S1,E1,E2,E3,S2,S3,LHS2,RHS2,IFIELD,IFIELD2,ITHIS,IMETHOD,IMETHOD2,ISTORE,ISTORE2])
),(
    add(ifT(IF, PA, ENC, IFLAG, THEN, 'null')),
        add(getFieldT(IFLAG, IF, ENC, null, 'flag', FLAG)),
        add(blockT(THEN, IF, ENC, [E1,E2,E3])),
                add(execT(E1,THEN,ENC,S1)),
            add(assignT(S1, E1, ENC, LHS2, RHS2)),
                add(getFieldT(LHS2, S1, ENC, IFIELD, 'flag', FLAG)),
                    add(getFieldT(IFIELD, LHS2, ENC, null,FNAME, FIELD)),
                add(literalT(RHS2, S1, ENC, type(basic, boolean, 0),'true')),
                add(execT(E2,THEN,ENC,S2)),
            add(applyT(S2, E2, ENC, ISTORE,'save',[IFIELD2],SAVE)),
                    add(identT(ISTORE, IMETHOD, ENC, 'Storage', STORE)),
                add(identT(IFIELD2, S2, ENC, FNAME, FIELD)),
                add(execT(E3,THEN,ENC,S3)),
            add(applyT(S3, E3, ENC, ISTORE2,'save',[ITHIS],SAVE)),
                    add(identT(ISTORE2, IMETHOD2, ENC, 'Storage', STORE)),
                add(identT(ITHIS, S3, ENC, 'this', CLASS)),
	    afterInBlock(ASSIGN, IF)
)).

action(afterInBlock(ID, After)) :-
%    printf('after(~a,~a).~n',[_id, _interID]),
	write(afterInBlock),
	flush_output,
	afterInBlock(ID, After).

afterInBlock(ID, After):-	
	tree(ID,Exec,_),
	tree(Exec,Block,_),
	blockT(Block,P,E,List),
    !,
    insertAfter(List,Exec,After,NewList),
    write(List),
    write(' '),
    write(NewList),
    replace(blockT(Block,P,E,List),	blockT(Block,P,E,NewList)).

afterInBlock(ID, After):-	
	format('could not add %a after %a in block',
	 [ID,After]).
	
insertAfter([],_,_,[]).
insertAfter([ID|List],ID,After,[ID|[After|List]]):-!.
insertAfter([First|List],ID,After,[First|NewList]):-
    not(First = ID),
	insertAfter(List,ID,After,NewList).
    
test(insertAfter):-
    assert_true(insertAfter([a,b,c],b,d,[a,b,d,c])).

% For every field (excluding arrays and basic types) add a 1<->1 back reference
%
% class A {
%   ...
%   B association;
%   ...
% }
%
% class B {
%   ...
%   >> A backref$A$association;
%   ...
% }
ct(addAssociationInterface, (
    % In every class
    classDefT(CLASS, _, _, _),not(externT(CLASS)),

    % ... with a field of a class type (no array)
    fieldDefT(VAR , CLASS, type(class, CLASS2, 0), NAME, _),
    % ... and this field is an association
    is_association(VAR),

    % ... and no generated back reference exists in the class
    pattern('backref$*$*',[CLASS,NAME],BNAME),
    not(fieldDefT(BVAR , CLASS2, type(class, CLASS, 0), BNAME, null)),
    
    new_id(BVAR)
),(
    % ... add a back reference field to the class
    add(fieldDefT(BVAR , CLASS2, type(class, CLASS, 0), BNAME, null))
)).


% For every association, where a backrefence exists in the referenced class,
% include code to set the backrefence after every assignment to the association:
% class A {
%   <method_in_A>() {
%     ...
%     association = newvalue
%     >> newvalue.backref$A$association = this
%     ...
%   }
% }
ct(addAssociationCode, (

    % in one class exists a reference
    fieldDefT(REF, CLASS, type(class, BCLASS, _), NAME, _),
    is_association(REF),
    classDefT(CLASS, _, _, _),

    % in the referenced class exists a back reference
    pattern('backref$*$*',[CLASS,NAME],BNAME),
    fieldDefT(BREF, BCLASS, type(class, CLASS, _), BNAME, _),
    classDefT(BCLASS, _, _, _),

    % in the first class is a writefield of the reference
    assignT(ASSIGN, PA, ENC, LHS, RHS),
       identT(LHS,ASSIGN,ENC,NAME,REF),

    % nothing else should happen after the write access - exclusively
    not(after(ASSIGN,_)),

    new_ids([BACKASSIGN, LHS2, RHS2])
),(
    add(assignT(BACKASSIGN, PA, ENC, LHS2, RHS2)),
        add(select(LHS2, BACKASSIGN, ENC, BNAME, RHS, BREF)),
        add(identT(RHS2, BACKASSIGN, ENC, 'this', CLASS)),

    add(after(ASSIGN, BACKASSIGN))
)).



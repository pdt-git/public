
tree(a) :- !.
tree(b) :- !.
tree(c) :- !.
tree(d) :- !.
tree(e) :- !.

action(e) :-
    action(d).       % preserve ... tree
action(r) :-
    action(a),       % preserve because tree(a) defined
    action(q),       % ignore because action(q) undefined
    action_all(c).   % preserve ... tree
action(r) :-
    action(b),       % preserve ... tree
    action(e),       % preserve ... tree
    action(f).       % expand recursively to d because of action(f) clause
action(f) :-
    action(d).

% TODO: The above facts and the expected result of next test clause don't match -- GK:  
test(expandAction1(_pef,_exp)) :- 
	tree(_pef), 
	expandAction(_pef,_exp), 
	assert_true('PEF term should be expanded to itself: ', 
	            (nonvar(_exp), _exp = [_pef]) ).
test(expandAction(_res)) :-  % nonexpandable
	findall(_exp, (tree(_pef),expandAction(_pef,_exp)), _res), 
	assert_true('PEF term should be expanded to itself without backtracking: ', 
	            (nonvar(_res), _res = [[_pef]]) ).
	            
test(expandAction(nonexpandable,_exp)) :- 
    expandAction(nonexpandable,_exp), 
	assert_true('Nonexpandable term should yield empty result list: ', 
	            (nonvar(_exp), _exp = []) ).
test(expandAction(nonexpandable-nobacktracking,_res)) :- 
    findall(_exp, expandAction(nonexpandable,_exp), _res), 
	assert_true('Nonexpandable term should yield empty result list without backtracking: ', 
	            (nonvar(_res) ,_res = [[]]) ).
    
test(expandAction(r,_res)) :- 
    findall(_exp, expandAction(r,_exp), _res),
 	assert_true('r should be expanded to : ', 
 	            (nonvar(_res), _res = [[a,c], [b,e,d]]) ).
 	
test('expandAndCollect#1') :- findall(_deps,expandAndCollect([a,s],[a,r],_deps),[[a], [a, c], [a, b, d], [a, d]]).
test('expandAndCollect#2') :- findall(_deps,expandAndCollect([],[a,r],_deps),[]).
test('expandAndCollect#3') :- findall(_deps,expandAndCollect([a,s],[],_deps),[]).

% TODO: The above facts and the expected result of next test clause don't match -- GK:   
test('expandActions#1') :- findall(_solution, expandActions([a,r],_solution), [[a, a, c], [a, b, e, d, d]]).
test('expandActions#2') :- findall(_solution, expandActions([],_solution), [[]]).
% TODO: The above facts and the expected result of next test clause don't match -- GK:   
test('expandActions#1') :- findall(_solution, expandActions([a,r],_solution), [[a, r, a, q, c], [a, r, b, e, d, f, d]]).
test('expandActions#2') :- findall(_solution, expandActions([],_solution), [[]]).


cond(s).
cond(c).
cond(u).
s :-
    b,
    u.
s :-
    a,
    c.
c :-
    d.
u :-
    d.
% Test of variant that eliminates expanded terms from the result:
test('expandConditions#1') :- findall(_solution, expandConditions([a,s],_solution), [[a, b, d], [a, a, d]]).
test('expandConditions#2') :- findall(_solution, expandConditions([],_solution), [[]]).

% Test of variant that leaves the expanded contition in the result:
%test('expandConditions#1') :- findall(_solution, expandConditions([a,s],_solution), [[a, s, b, u, d], [a, s, a, c, d]]).
%test('expandConditions#2') :- findall(_solution, expandConditions([],_solution), [[]]).


:- module(transaction_test,[]).

:- use_module(library('/spike/transaction')).
:- dynamic test_fact/1.
my_setup:-
    retractall(test_fact(_)).

my_teardown.    

:- begin_tests(transaction,[]).



test(simple_succeed,[setup(my_setup),cleanup(my_teardown)]):-
    pdt_transaction(
    	(	pdt_assert(test_fact(1)),
    		pdt_assert(test_fact(2)),
    		pdt_assert(test_fact(3))
    	)
    ),
    findall(A,test_fact(A),As),
    As==[1,2,3].

test(simple_fail,[setup(my_setup),cleanup(my_teardown)]):-
    
    (	pdt_transaction(
    		(	pdt_assert(test_fact(4)),
    			pdt_assert(test_fact(5)),
    			pdt_assert(test_fact(6)),
    			fail
    		)
    	)
    ->	fail
    ;	true
    ),
    findall(A,test_fact(A),As),
    As==[].

test(simple_bt,[setup(my_setup),cleanup(my_teardown)]):-
    
	pdt_transaction(
		(	pdt_assert(test_fact(7)),
			(	pdt_assert(test_fact(8)),
				fail
			;	pdt_assert(test_fact(9))
			)
			
		)
	),
    findall(A,test_fact(A),As),
    As==[7,9].



test(reflective_1,[setup(my_setup),cleanup(my_teardown)]):-
    pdt_transaction(
    	(	pdt_assert(test_fact(10)),
    		pdt_assert(test_fact(11)),
    		pdt_assert(test_fact(12)),
    		findall(X,pdt_query(test_fact(X)),Xs),
    		pdt_assert(test_fact(Xs))
    	)
    ),
    findall(A,test_fact(A),As),
    As==[10,11,12,[10,11,12]].


test(reflective_2,[setup(my_setup),cleanup(my_teardown)]):-
    pdt_transaction(
    	(	pdt_assert(test_fact(13)),
    		pdt_assert(test_fact(14)),
    		pdt_assert(test_fact(15))    		
    	)
    ),
    pdt_transaction(
    	(	findall(X,pdt_query(test_fact(X)),Xs),
    		pdt_assert(test_fact(Xs))
    	)
    	
    ),
    findall(A,test_fact(A),As),
    As==[13,14,15,[13,14,15]].

test(reflective_2,[setup(my_setup),cleanup(my_teardown)]):-
    pdt_transaction(
    	(	pdt_assert(test_fact(16)),
    		pdt_assert(test_fact(17))
    	)
    ),
    pdt_transaction(
    	(	pdt_assert(test_fact(18)),
	    	pdt_assert(test_fact(19)),
    		findall(X,pdt_query(test_fact(X)),Xs),
    		pdt_assert(test_fact(Xs))
    	)
    	
    ),
    findall(A,test_fact(A),As),
    As==[16,17,18,19,[16,17,18,19]].

test(twice,[setup(my_setup),cleanup(my_teardown)]):-
    pdt_transaction(
    	(	pdt_assert(test_fact(20))
    	)
    ),

    pdt_transaction(
    	(	pdt_assert(test_fact(21))			    
    	)
    ),
    findall(A,test_fact(A),As),
    As==[20,21].



:- end_tests(transaction). 
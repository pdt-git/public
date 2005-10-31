% Author: Guenter Kniesel
% Date: 19.09.2005

/** 
 * Quick comparison of two versions of a predicate
 * that are supposed to give the same result. Call
 * regressionTest/2 and look for ' --  fail ' strings in 
 * the output in order to detect mismatches of the results.
 *
 * Assumption: Defensive programming, that is creation of a
 * new version of a predicate instead of overwriting the old
 * version. You can still overwrite the old version after 
 * successful run of your regression test.
 *
 * Example:
 * regressionTest( newTreeSignature(F, Arity), treeSignature(F, Arity) ).
 */

regressionTest(Old,New) :- equiv( Old, New ).
regressionTest(_ld,_ew) :- nl, fail.
regressionTest(Old,New) :- equiv( New, Old ).
regressionTest(_ld,_ew) :- nl.

equiv(P,Q) :-
   call(P),
   ( (write(P), write(' -- ')) 
   ; (write(' fail '), nl, fail)
   ),
   call(Q),
   write(Q), 
   fail.



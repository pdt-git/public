/* 
  Start by defining criterion what is to be generated (expanded by
  inlining definitions of body literals) and what is to be eliminated
  (all occurences inlined).
  
  Example:
  
  a(X,Y) :- b(X,Z), c, d(X,Y), e(Y,Z).
  
  b(a,1).
  b(b,2).
  
  c.
  
  100 definitions of d(X,Y).
  100 definitions of e(X,Y).
  
  In this example a can be expanded by eliminating b and c
  but not d and e (which would cause a combinatorial explosion of facts):
  
  a(a,Y) :- d(a,Y), e(Y,1).
  a(b,Y) :- d(b,Y), e(Y,2).
  
  If general criteria are too difficult, just add some jTransformer-specific 
  metainformation. Argument selectivity!
  
*/
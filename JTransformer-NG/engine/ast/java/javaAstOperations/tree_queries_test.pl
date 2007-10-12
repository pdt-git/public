test(subtype):-
    subtype(int,long),
    subtype(int,int),
    subtype(void,void),
    subtype(Basic11,int),
    Basic11=char,
    subtype(Basic21,Basic22),
    Basic21=char,
    Basic22=int.
    
    
test(subtype1):-
    fullQualifiedName(Object,'java.lang.Object'),
    subtype(String, Object),
    fullQualifiedName(String,'java.lang.String').

test(subtype2):-
    fullQualifiedName(Object,'java.lang.Object'),
    fullQualifiedName(String,'java.lang.String'),
    subtype(String, Object).

test(subtype3):-
    subtype(String, Object),
    fullQualifiedName(Object,'java.lang.Object'),
    fullQualifiedName(String,'java.lang.String').

test(subtype4):-
    fullQualifiedName(Object,'java.lang.Object'),
    subtype(Object, Object).
    
test(subtype5):-
    fullQualifiedName(Object,'java.lang.Object'),
    not(subtype(int, Object)).

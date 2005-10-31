% nach: Bob Morein, A.D.A.-Prolog 1985
uniquequicksort(_list, _List) :-
    quicksort(_list,_List, []).

quicksort([Head|Tail], SortierteListe, X ) :-
        split( Head, Tail, Kleiner, Groesser),
        quicksort(Kleiner, SortierteListe, [Head|Rest]),
        quicksort(Groesser, Rest, X ).
quicksort( [], Liste, Liste ).
        split(_, [], [], []):-!.
        split(Element, [Head|Tail], [Head|Kleiner], Groesser):-
                compare(<,Head,Element), !,
                split(Element, Tail, Kleiner, Groesser).
        split(Element, [Head|Tail], Kleiner, Groesser):-
                compare(=,Head,Element), !,
                split(Element, Tail, Kleiner, Groesser).
        split(Element, [Head|Tail], Kleiner, [Head|Groesser]):-
                split(Element, Tail, Kleiner, Groesser).

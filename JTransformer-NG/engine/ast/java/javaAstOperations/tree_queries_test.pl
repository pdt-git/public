setUp(walkTree1):-
applyT(17905, 17904, 10113, null, super, [], 10090).
execT(17904, 17903, 10113, 17905).
blockT(17903, 10113, 10113, [17904]).
methodDefT(10113, 10110, <init>, [], type(basic, void, 0), [], 17903).
modifierT(10113, public).
modifierT(10113, synthetic).
fieldDefT(10117, 10110, type(class, 10133, 0), inbox, null).
modifierT(10117, private).



getFieldT(17918, 17917, 10118, null, inbox, 10117).


returnT(17917, 17916, 10118, 17918).

blockT(17916, 10118, 10118, [17917]).

methodDefT(10118, 10110, getInbox, [], type(class, 10133, 0), [], 17916).
modifierT(10118, public).



literalT(17924, 17923, 10119, type(basic, int, 0), 1).

literalT(17925, 17923, 10119, type(basic, int, 0), 2).

literalT(17926, 17923, 10119, type(basic, int, 0), 3).

literalT(17927, 17923, 10119, type(basic, int, 0), 4).

literalT(17928, 17923, 10119, type(basic, int, 0), 5).

newArrayT(17923, 17921, 10119, [], [17924, 17925, 17926, 17927, 17928], type(basic, int, 1)).

localDefT(17921, 17920, 10119, type(basic, int, 1), a, 17923).


paramDefT(17929, 17922, type(class, 10001, 0), ob).


identT(17930, 17922, 10119, a, 17921).


identT(17936, 17934, 10119, System, 10023).

getFieldT(17934, 17933, 10119, 17936, out, 10156).


literalT(17935, 17933, 10119, type(class, 10005, 0), hello).

applyT(17933, 17932, 10119, 17934, println, [17935], 10154).


execT(17932, 17931, 10119, 17933).

blockT(17931, 17922, 10119, [17932]).

foreachT(17922, 17920, 10119, 17929, 17930, 17931).

blockT(17920, 10119, 10119, [17921, 17922]).

paramDefT(17919, 10119, type(class, 10005, 1), args).


methodDefT(10119, 10110, main, [17919], type(basic, void, 0), [], 17920).
modifierT(10119, public).
modifierT(10119, static).




classDefT(10110, 10080, MailClient, [10113, 10117, 10118, 10119]).
extendsT(10110, 10001).
modifierT(10110, public).



identT(17909, 17908, 10110, org.cs3.pimpro.Activator, type(class, 10124, 0)).
selectT(17908, 17906, 10110, class, 17909, type(class, 10002, 0)).


memberValueT(17906, 17907, null, 17908).
annotationT(17907, 10110, 10110, 10111, [17906]).
annotatedT(10110, 17907).
annotatedT(10110, 17910).

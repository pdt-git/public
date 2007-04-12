setUp(walkTree1):-
applyT(17905, 17904, 10113, null, super, [], 10090).r
execT(17904, 17903, 10113, 17905).r
blockT(17903, 10113, 10113, [17904]).r
methodDefT(10113, 10110, <init>, [], type(basic, void, 0), [], 17903).r
modifierT(10113, public).r
modifierT(10113, synthetic).r
fieldDefT(10117, 10110, type(class, 10133, 0), inbox, null).r
modifierT(10117, private).r



getFieldT(17918, 17917, 10118, null, inbox, 10117).r


returnT(17917, 17916, 10118, 17918).r

blockT(17916, 10118, 10118, [17917]).r

methodDefT(10118, 10110, getInbox, [], type(class, 10133, 0), [], 17916).r
modifierT(10118, public).r



literalT(17924, 17923, 10119, type(basic, int, 0), 1).r

literalT(17925, 17923, 10119, type(basic, int, 0), 2).r

literalT(17926, 17923, 10119, type(basic, int, 0), 3).r

literalT(17927, 17923, 10119, type(basic, int, 0), 4).r

literalT(17928, 17923, 10119, type(basic, int, 0), 5).r

newArrayT(17923, 17921, 10119, [], [17924, 17925, 17926, 17927, 17928], type(basic, int, 1)).r

localDefT(17921, 17920, 10119, type(basic, int, 1), a, 17923).r


paramDefT(17929, 17922, type(class, 10001, 0), ob).r


identT(17930, 17922, 10119, a, 17921).r


identT(17936, 17934, 10119, System, 10023).r

getFieldT(17934, 17933, 10119, 17936, out, 10156).r


literalT(17935, 17933, 10119, type(class, 10005, 0), hello).r

applyT(17933, 17932, 10119, 17934, println, [17935], 10154).r


execT(17932, 17931, 10119, 17933).r

blockT(17931, 17922, 10119, [17932]).r

foreachT(17922, 17920, 10119, 17929, 17930, 17931).r

blockT(17920, 10119, 10119, [17921, 17922]).r

paramDefT(17919, 10119, type(class, 10005, 1), args).r


methodDefT(10119, 10110, main, [17919], type(basic, void, 0), [], 17920).r
modifierT(10119, public).r
modifierT(10119, static).r




classDefT(10110, 10080, MailClient, [10113, 10117, 10118, 10119]).r
extendsT(10110, 10001).r
modifierT(10110, public).r



identT(17909, 17908, 10110, org.cs3.pimpro.Activator, type(class, 10124, 0)).r
selectT(17908, 17906, 10110, class, 17909, type(class, 10002, 0)).r


memberValueT(17906, 17907, null, 17908).r
annotationT(17907, 10110, 10110, 10111, [17906]).r
annotatedT(10110, 17907).r
annotatedT(10110, 17910).r

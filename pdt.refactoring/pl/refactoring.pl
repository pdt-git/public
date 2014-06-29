:- module('refactoring',
         [ rename/3              % (Selection, NewName, TextChanges)
         ]
         ).


/**
 * rename(+Selection, +NewText, ?TextChanges) 
 *
 *		* Selection is a list: [File, Offset, Length]
 *			(The length of the selection can be derived from the Text
 *		* NewText is the String of the new name that the user is asked to input
 *		* TextChange is a list that will be expected in the format:
 *				[File, Offset, Length, NewText] 
 *
 *		Note that the predicate should succeed multiple times, for multiple text changes!   
 */
rename(Selection, NewText, TextChange):-
	Selection = [File, OldOffset, OldLength],
	
	% Do some Magic here and calculate the TextChanges
	
	TextChange = [File, OldOffset, OldLength, NewText],
	true.
   
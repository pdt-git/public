:- module(readws, [
	readws_file_to_sentence_pltexts/2]).

%------------------------------------------------------------------------------

% flatten( +List, -FlatList) :-
%	from the SWI-Prolog library
%	(it used to be built-in)

flatten(List, FlatList) :-
	flatten(List, [], FlatList0), !,
	FlatList = FlatList0.


flatten( Var, Tl, [Var|Tl]) :-
	var( Var),
	!.

flatten( [], Tl, Tl) :-
	!.

flatten( [Hd|Tl], Tail, List) :-
	flatten( Hd, FlatHeadTail, List), 
	flatten( Tl, Tail, FlatHeadTail).

flatten( Atom, Tl, [Atom|Tl]).

%------------------------------------------------------------------------------

% member( -Item, +List) :-
%	formerly a SWI-Prolog built-in

member( X, [X|_]).

member( X, [_|T]) :-
	member( X, T).

%------------------------------------------------------------------------------

% append( +List1, +List2, -List) :-
%	formerly a SWI-Prolog built-in

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).


/* pred
	append(list(list(T)), list(T)),
	append(list(T), list(T), list(T), list(T), list(T)),
	correspond(T, list(T), list(U), U),
	delete(list(T), T, list(T)),
	delete(list(T), T, integer, list(T)),
	    delete_1(list(T), T, integer, list(T)),
	is_list(T),
	keys_and_values(list(pair(T,U)), list(T), list(U)),
	last(T, list(T)),
	    last_1(list(T), T, T),
	nextto(T, T, list(T)),
	nmember(T, list(T), integer),
	nth0(integer, list(T), T),
	nth1(integer, list(T), T),
	    nth0v(list(T), T, integer, integer),
	    nth0i(integer, list(T), T),
	nth0(integer, list(T), T, list(T)),
	nth1(integer, list(T), T, list(T)),
	    nth0i(integer, list(T), T, list(T)),
	    nth0v(list(T), T, integer, integer, list(T)),
	one_longer(list(T), list(T)),
	perm(list(T), list(T)),
	    insert(list(T), T, list(T)),
	permutation(list(T), list(T)),
	    permutation(list(T), list(T), list(T)),
	perm2(T, T, T, T),
	proper_length(list(T), integer),
	    proper_length(list(T), integer, integer),
	remove_dups(list(T), list(T)),
	rev(list(T), list(T)),
	    rev(list(T), list(T), list(T)),
	reverse(list(T), list(T)),
	    reverse(list(T), list(T), list(T), list(T)),
	same_length(list(T), list(T)),
	same_length(list(T), list(T), integer),
	    'same length'(integer, list(T), list(T)),
	    'same length'(list(T), list(T), integer, integer),
	select(T, list(T), T, list(T)),
	selectchk(T, list(T), T, list(T)),
	shorter_list(list(T), list(T)),
	subseq(list(T), list(T), list(T)),
	subseq0(list(T), list(T)),
	subseq1(list(T), list(T)),
	sumlist(list(integer), integer),
	    sumlist(list(integer), integer, integer),
	transpose(list(list(T)), list(list(T))),
	    transpose(list(list(T)), list(list(T)), list(list(T))),
		transpose_1(list(U), list(U)),
		transpose_1(list(list(T)), list(T), list(list(T))).
*/


%   append(+ListOfLists, ?List)
%   is true when ListOfLists is a list [L1,...,Ln] of lists, List is
%   a list, and appending L1, ..., Ln together yields List.  The
%   ListOfLists **must** be a proper list.  (Strictly speaking we
%   should produce an error message if it is not, but this version
%   fails.)  Additionally, either List should be a proper list, or
%   each of L1, ..., Ln should be a proper list.  The behaviour on
%   non-lists is undefined.  ListOfLists must be proper because for
%   any given solution, infinitely many more can be obtained by
%   inserting nils ([]) into ListOfList.

append(-, _) :- !, fail.	% reject partial lists.
append([], []).
append([L|Ls], List0) :-
	append(L, List1, List0),
	append(Ls, List1).



%   append(?Prefix, ?Tail1, ?List1, ?Tail2, ?List2)
%   is true when append(Prefix, Tail1, List1) and append(Prefix, Tail2, List2)
%   are both true.  You could call append/3 twice, but that is order-
%   dependent.  This will terminate if Prefix is a proper list or if
%   either List1 or List2 is a proper list.

append([], List1, List1, List2, List2).
append([H|T], Tail1, [H|List1], Tail2, [H|List2]) :-
	append(T, Tail1, List1, Tail2, List2).
%------------------------------------------------------------------------------

% readws_based( -CsBased, +Base, +N0, -N, -C0, +Cs1, -Cs0) :-
%	read an integer in base Base

readws_based( CsBased, Base, N0, N, C0, [C1|Cs2], Cs0) :-
	(	C1 >= "0",
		C1 =< "9"
	->	Digit is C1-"0"
	;	C1 >= "A",
		C1 =< "Z"
	->	Digit is C1-("A"-10)
	;	C1 >= "a",
		C1 =< "z"
	->	Digit is C1-("a"-10)
	;	Digit is 99		% C1 will be spurned (below) unless it's an underscore (?!)
	),
	(	Digit < Base
	->	N1 is N0*Base + Digit,
		readws_based( CsBased1, Base, N1, N, C0, Cs2, Cs0),
		CsBased = [C1|CsBased1]
	;	C1 =:= "_"		% overlook (?!) underscores in (based) integers
	->	readws_based( CsBased1, Base, N0, N, C0, Cs2, Cs0),
		CsBased = [C1|CsBased1]
	;	CsBased = [],
		N0 = N,
		C1 = C0,
		Cs2 = Cs0
	).

%------------------------------------------------------------------------------

% readws_char( -Cchar, -CsSrc1, +CsSrc0, -Errs, +Quote, ?Env, +C1, -C0, +Cs1, -Cs0) :-
%	reads a single `character' from a string, quoted atom, or character constant.
%	C1 is the first character it is to look at.
%	Quote is the surrounding quotation mark, which is " for strings,
%	' for quoted atoms, and the radix character (also ') for character constants.
%	A Dec-10 Prolog incompatibility is that	it does not allow newline characters
%	in strings unless they are preceded by an escape character.
%	As reading an extended character would sometimes read one character too many,
%	it is made to do so always, and to return the first character which does not belong in
%	the character as C0.
%	When we have hit the end of the string (whether cleanly of by premature EOF),
%	we return Cchar = -1.

readws_char( Cchar, CsSrc1, CsSrc0, Errs, Quote, E, C1, C0, Cs1, Cs0) :-
	(	C1 =:= 92,									% 92 is backslash
		memberchk( character_escapes-yes, E)		% we must interpret character escapes
	->	Cs1 = [C2|Cs2],
		(	C2 =:= -1								% end-of-file already
		->	Cchar is -1,
			CsSrc1 = [C1|CsSrc0],					% the backslash is the only sourcetext
			readws_quote_context( Quote, Context),
			Err = eof(esc(Context)),
		 	Errs = [Err],
			memberchk( status-Err, E),
		 	C0 = C2,
		 	Cs0 = Cs2
		;	C2 =< " "								% \<layout> is skipped...
		->	CsSrc1 = [C1,C2|CsSrc2],				% ...but is part of the sourcetext
			Cs2 = [C3|Cs3],
			readws_char( Cchar, CsSrc2, CsSrc0, Errs, Quote, E, C3, C0, Cs3, Cs0)
		;	C2\/32 =:= "c"							% any ASCII or Latin-1 'c'
		->	/* \c<layout>* is skipped; to get a blank after this */
			/* do e.g. "...\c		\ <space>" where the "\ " ends */
			/* the skipping and the NEXT blank is taken.  */
			/* ?? so don't we need some gadget to consume subsequent layout? */
			CsSrc1 = [C1,C2|CsSrc2],
			Cs2 = [C3|Cs3],
			readws_char( Cchar, CsSrc2, CsSrc0, Errs, Quote, E, C3, C0, Cs3, Cs0)
		;	C2 =< "7",
			C2 >= "0"		% \<1-3 octal digits> (hairy bit: \1234 is S4)
		->	CsSrc1 = [C1,C2|CsSrc2],
			Cs2 = [C3|Cs3],
			(	C3 =< "7",
				C3 >= "0"
			->	CsSrc2 = [C3|CsSrc3],
				Cs3 = [C4|Cs4],
				(	C4 =< "7",
					C4 >= "0"
				->	CsSrc3 = [C4|CsSrc0],
					Cs4 = [C0|Cs0],
					Cchar is (C2*8+C3)*8+C4 - 73*"0"
				;	CsSrc3 = CsSrc0,
					C4 = C0,
					Cs4 = Cs0,
					Cchar is (C2*8+C3) - 9*"0"
			 	)
			;	CsSrc2 = CsSrc0,
				C3 = C0,
				Cs3 = Cs0,
				Cchar is C2-"0"
			),
			Errs = []
		;	C2 =:= "^"								% escaped control char (trigram)
		->	Cs2 = [C3|Cs3],
			(	C3 =:= -1							% end-of-file (prematurely)
			->	Cchar is -1,
				CsSrc1 = [C1,C2|CsSrc0],
				readws_quote_context( Quote, Context),
				Err = eof(esc_ctrl(Context)),
			 	Errs = [Err],
				memberchk( status-Err, E),
			 	C0 = C3,
			 	Cs0 = Cs3
			;	C3 =:= "?"
			->	Cchar = 127,		% \^? = DEL
				CsSrc1 = [C1,C2,C3|CsSrc0],
				Errs = [],
				Cs3 = [C0|Cs0]
			;	Cchar is C3/\31,	% \^X -> control-X, for example
				CsSrc1 = [C1,C2,C3|CsSrc0],
				Errs = [],
				Cs3 = [C0|Cs0]
			)
		;	readws_escape_char( C2, Cchar)			% a regular escape char, representing Cchar
		->	CsSrc1 = [C1,C2|CsSrc0],
			Errs = [],
			Cs2 = [C0|Cs0]
		;	Cchar = C2,			% probably "'", '"',  or \ itself (or redundant escaping of anything else)
			CsSrc1 = [C1,C2|CsSrc0],
			Errs = [],
			Cs2 = [C0|Cs0]
		)
	;	C1 =:= Quote					% single- or double-quoted string, but not character const
	->	Cs1 = [C2|Cs2],
		(	C2 =:= Quote				% a doubled quote...
		->	Cchar = Quote,				% ...denotes itself
			CsSrc1 = [C1,C2|CsSrc0],
			Cs2 = [C0|Cs0]
		;	Cchar is -1,					% else it terminates the sequence
			CsSrc1 = [C1|CsSrc0],
			C0 = C2,
			Cs0 = Cs2
		),
		Errs = []
	;	C1 =:= -1						% end-of-file...
	->	Cchar is -1,					% ...terminates the sequence...
		CsSrc1 = CsSrc0,
		readws_quote_context( Quote, Context),
		Err = eof(Context),
		Errs = [Err],					% ...abnormally
		memberchk( status-Err, E),
		C0 = C1,
		Cs0 = Cs1
	;	readws_junk_code( C1)			% a non-layout control character
	->	Cchar = C1,						% return it...
		CsSrc1 = [C1|CsSrc0],
		Errs = [bad_ctrl(Cchar)],		% ...flagged as an error
		Cs1 = [C0|Cs0]
	;	Cchar = C1,						% accept anything else
		CsSrc1 = [C1|CsSrc0],
		Errs = [],
		Cs1 = [C0|Cs0]
	).

%------------------------------------------------------------------------------

readws_codes_to_sentence_pltexts( Cs, Ps) :-
	readws_codes_to_tokens( Cs, Ts),
	readws_tokens_to_sentence_tokenss( Ts, Tss),
	maplist( readws_tokens_to_pltext, Tss, Ps).

%------------------------------------------------------------------------------

% readws_codes_to_tokens( +Codes, -Tokens, -Dict, -Status) :-
%	binds Tokens to a list of t/4 tokens, and Dict to a dictionary of Name=Variable pairs in standard order
%	(where the Names are atoms and the variables are all the distinct _named_ variables among the tokens)
%	NB the append/3 call inefficiently copies the entire list of codes;
%	Codes should be a list(integer), each in 0..127 (ASCII) or 0..255 (Latin-1).
%	Status is one of:
%		bad_nth_code(N,C)
%			if the N-th code (C, numbered from 1) of Codes is outside 0..255
%		eof(comment(c1))
%			if end-of-file terminated a %comment
%		eof(comment(c2))
%			if end-of-file prematurely terminated a /*comment*/
%		eof(quotes(single))
%			if end-of-file prematurely terminated a quoted atom
%		eof(quotes(double))
%			if end-of-file prematurely terminated a string
%		eof(char_const)
%			if end-of-file prematurely terminated a character code constant
%		eof(esc(quotes(single)))
%			if end-of-file prematurely terminated an escape sequence (e.g. \n) in single quotes
%		eof(esc(quotes(double)))
%			if end-of-file prematurely terminated an escape sequence (e.g. \n) in double quotes
%		eof(esc(char_const)))
%			if end-of-file prematurely terminated an escape sequence (e.g. \n) in a character code constant
%		eof(esc_ctrl(quotes(single)))
%			if end-of-file prematurely terminated an escaped control sequence (e.g. \^X) in single quotes
%		eof(esc_ctrl(quotes(double)))
%			if end-of-file prematurely terminated an escaped control sequence (e.g. \^X) in double quotes
%		eof(esc_ctrl(char_const)))
%			if end-of-file prematurely terminated an escaped control sequence (e.g. \^X) in a character code constant
%		ok
%			if tokenising completed ok (perhaps with some errors flagged within t/4 structures)
%
%	Implementation notes:
%	Env may acquire further pairs, but when it stabilises, it could be refactored as a compound term.


readws_codes_to_tokens( Cs, Ts) :-
	readws_codes_to_tokens( Cs, Ts, _Dict).


readws_codes_to_tokens( Cs, Ts, Dict) :-
	readws_codes_to_tokens( Cs, Ts, Dict, _Status).


readws_codes_to_tokens( Cs, Ts, Dict, S) :-
	readws_scan_codes( Cs, [C1|Cs1], S1),	% appends EOF marker
	(	integer( S1)						% S1 is 7 or 8 (data width)
	->	Env = [
			dict - Dict1,
			character_escapes - yes,		% we may need to support variants?
			status - S
		],
		(	readws_tokens_1( C1, Env, Ts1, Cs1)
		->	readws_terminate_list( Dict1),	% fill in the "hole" at the end
			sort( Dict1, Dict),
			(	var( S)
			->	S = ok						% although Tokens may contain errors
			;	true
			),
			Ts = Ts1
		;	S = failed(readws_tokens_1/4)	% cannot happen (?)
		)
	;	S = S1								% bad character code data
	).

%------------------------------------------------------------------------------

% readws_comment( -CsComment, ?Env, +C1, -C0, +Cs1, -Cs0) :-
%	C1 has already been determined to be part of a %comment;
%	CsComment is the complete remaining comment text,
%	from C1 up to but not including the first end-of-line or end-of-file,
%	which is returned as C0

readws_comment( [C1|CsComment1], E, C1, C0, [C2|Cs2], Cs0) :-
	(	readws_eol( C2)			% an end-of-line character
	->	CsComment1 = [],
		C2 = C0,
		Cs2 = Cs0
	;	C2 =:= -1				% the end-of-file marker
	->	CsComment1 = [],
		memberchk( status-eof(comment(c1)), E),
		C2 = C0,
		Cs2 = Cs0
	;	readws_comment( CsComment1, E, C2, C0, Cs2, Cs0)
	).

%------------------------------------------------------------------------------

% readws_eol( -Code) :-
%	Code encodes the 'end-of-line' character

readws_eol( 10).

%------------------------------------------------------------------------------

% readws_escape_char( -Escapee, -Replacement) :-
%	This table is for ASCII.
%	On Xerox Lisp systems, \n should map to 13 (CR).
%	The whole table needs replacing in EBCDIC systems (in which the assumption that A..Z and a..z are contiguous blocks also needs correcting).

readws_escape_char( 0'n,  10).		% \n = new line (NL)
readws_escape_char( 0'N,  10).		% \N = new line (NL)
readws_escape_char( 0't,   9).		% \t = horizontal tab (HT)
readws_escape_char( 0'T,   9).		% \T = horizontal tab (HT)
readws_escape_char( 0'r,  13).		% \r = carriage return (CR)
readws_escape_char( 0'R,  13).		% \R = carriage return (CR)
readws_escape_char( 0'v,  11).		% \v = vertical tab (VT)
readws_escape_char( 0'V,  11).		% \V = vertical tab (VT)
readws_escape_char( 0'b,   8).		% \b = backspace (BS)
readws_escape_char( 0'B,   8).		% \B = backspace (BS)
readws_escape_char( 0'f,  12).		% \f = form feed (FF)
readws_escape_char( 0'F,  12).		% \F = form feed (FF)
readws_escape_char( 0'e,  27).		% \e = escape (ESC)
readws_escape_char( 0'E,  27).		% \E = escape (ESC)
readws_escape_char( 0'd, 127).		% \d = delete (DEL)
readws_escape_char( 0'D, 127).		% \D = delete (DEL)
readws_escape_char( 0's,  32).		% \s = space
readws_escape_char( 0'S,  32).		% \S = space

% readws_escape_char( 0'z, -1).		% \z = end of file (?!)
% readws_escape_char( 0'Z, -1).		% \Z = end of file (?!)

%------------------------------------------------------------------------------

% readws_exponent( -CsExponent, +CExp, -C0, +Cs1, -Cs0) :-
%	reads a maximal sequence CsExponent of one or more decimal digits
%	from [CExp|Cs1] leaving [C0|Cs0],
%	where CExp has already been found to be such a digit

readws_exponent( [CExp|CsExpRest], CExp, C0, [C2|Cs2], Cs0) :-
	(	C2 >= "0",		% ASCII decimal digit
		C2 =< "9"
	->	readws_exponent( CsExpRest, C2, C0, Cs2, Cs0)
	;	CsExpRest = [],
		C2 = C0,
		Cs2 = Cs0
	).

%------------------------------------------------------------------------------

% readws_file_to_sentence_pltexts( +FileName, -PlTexts) :-

readws_file_to_sentence_pltexts( F, Ps) :-
	file_to_chars_v5( F, Cs),
	readws_codes_to_sentence_pltexts( Cs, Ps).

%------------------------------------------------------------------------------

% readws_float( -Float, -Errs, -CsFract, +CsIntPart, +CFrac, -C0, +Cs1, -Cs0) :-
%	is called when we have parsed <digit>* "." <digit>;
%	CsIntPart encodes (canonically) the value of the integral part (already recognised);
%	CFract is the first digit after the decimal point;
%	Float is the float value of the entire number;
%	Errs is [] or [bad_exponent]
%	CsFract encode the characters representing the fractional and exponent part

readws_float( Float, Errs, CsFract, CsInt, CFract, C0, Cs1, Cs0) :-
	readws_float_1( CsFract, Errs, CFract, C0, Cs1, Cs0),
	(	member( Errs, float_no_exponent)
	->	flatten( [CsInt,".",CsFract,"0"], CsFloat)	% fake an exponent of zero for evaluation
	;	flatten( [CsInt,".",CsFract], CsFloat)
	),
	number_codes( Float, CsFloat).					% compute the overall value

%------------------------------------------------------------------------------

% readws_float_1( -CsFract, -Errs, +C1, -C0, +Cs1, -Cs0) :-
%	C1 has already been found to be a fractional digit;
%	(any) further digits, and (optional) exponent part, are read from Cs1

readws_float_1( [C1|CsFract1], Errs, C1, C0, [C2|Cs2], Cs0) :-
	(	C2 >= "0",
		C2 =< "9"
	->	readws_float_1( CsFract1, Errs, C2, C0, Cs2, Cs0)
	;	C2\/32 =:= "e"		% i.e. "e" or "E" (ASCII or Latin-1)
	->	Cs2 = [C3|Cs3],
		(	C3 =:= "-"
		->	Cs3 = [C4|Cs4],
			CsFract1 = [C2,0'-|CsFract2]
		;	C3 =:= "+"
		->	Cs3 = [C4|Cs4],
			CsFract1 = [C2,0'+|CsFract2]
		;	C3 = C4,
			CsFract1 = [C2|CsFract2],
			Cs3 = Cs4
		),
		(	C4 >= "0",
			C4 =< "9"
		->	Errs = [],
			readws_exponent( CsFract2, C4, C0, Cs4, Cs0)
		;	Errs = [float_no_exponent],
			CsFract2 = "",
			C4 = C0,
			Cs4 = Cs0
		)
	;	CsFract1 = [],
		Errs = [],
		C2 = C0,
		Cs2 = Cs0
	).

%------------------------------------------------------------------------------

% readws_fullstop( +C1, -Fs, -Fs1, ?Env, -Token, +Cs1, -C0, -Cs0) :-
%	we've already consumed a period, which might:
%	 * introduce a degenerate float e.g. .9
%	 * be the start of a symbolic atom which begins with a period
%	 * be a fullstop

readws_fullstop( C1, Fs, Fs1, E, T, Cs1, C0, Cs0) :-
	(	C1 =< "9",
		C1 >= "0"
	->	readws_float( Float, Errs, CsFract, "0", C1, C0, Cs1, Cs0),	% pretend we've seen an integral part of "0"
		T = t(number,[float_no_int_part|Errs],Float,Fs),
		Fs1 = [p*[0'.|CsFract]]	% prepend the already-seen . to the fractional part's digits
	;	C1 > " "							% ordinary token starting with "."
	->	readws_rest_symbol( CsRestSymbol, C1, C0, Cs1, Cs0),
		CsSymbol = [0'.|CsRestSymbol],
		atom_codes( Symbol, CsSymbol),
		Fs1 = [p*CsSymbol],
		T = t(atom,[],Symbol,Fs)
	;	Fs1 = [p*"."|Fs2],
		readws_post_eos( Fs2, E, C1, Cs1, C0, Cs0),
		T = t(eos,[],'.',Fs)				% end-of-sentence
	).

%------------------------------------------------------------------------------

% readws_identifier( +C1, -Fs, -Fs1, ?Env, -Tokens, +Cs1) :-
%	reads an atom which begins with the lower case letter C1
%	and continues with letters, digits, and underscores,
%	then gets remaining tokens

readws_identifier( C1, Fs, [p*CsName], E, [t(atom,[],Name,Fs)|Ts], Cs1) :-
	readws_name( CsName, C1, C2, Cs1, Cs2),
	atom_codes( Name, CsName),
	readws_tokens_1( C2, E, Ts, Cs2).

%------------------------------------------------------------------------------

% readws_inline_solidus( -CsC, ?Env, +C1, +Cs1, -C0, -Cs0) :-

readws_inline_solidus( CsC, E, C1, Cs1, C0, Cs0) :-
	C1 =:= 0'/,
	readws_solidus( CsC, 1, 1, E, C1, C0, Cs1, Cs0).

%------------------------------------------------------------------------------

% readws_junk( +CJunk, +Cs1, -CsJunk, -Fs1, +Fs0, -C0, -Cs0) :-
%	consumes a sequence CsJunk of one or more "junk" character codes
%	from [CJunk|Cs1]
%	leaving [C0|Cs0]

readws_junk( CJ, [C1|Cs1], [CJ|CsJ], Fs1, Fs0, C0, Cs0) :-
	(	readws_junk_code( C1)
	->	readws_junk( C1, Cs1, CsJ, Fs1, Fs0, C0, Cs0)
	;	CsJ = [],
		readws_layout( C1, Cs1, Fs1, Fs0, C0, Cs0)
	).

%------------------------------------------------------------------------------

% readws_junk_code( +C) :-
%	C encodes a character which we regard as junk,
%	i.e. any ASCII or Latin-1 control character (or DEL) other than
%	those we accept as whitespace

readws_junk_code( C) :-
	(	C >= 0,			% ASCII control
		C < 32,
		\+ readws_whitespace_code( C)
	;	C >= 127,		% ASCII DEL or Latin-1 extended control
		C < 160
	).

%------------------------------------------------------------------------------

% readws_layout( +C1, +Cs1, -Fs1, +Fs0, -C0, -Cs0) :-
%	consumes a maximal sequence of maximal whitespace or junk strings
%	from [C1|Cs1]
%	appending w*CsWhiteSpace or j*CsJunk as appropriate to Fs1-Fs0
%	leaving [C0|Cs0]

readws_layout( C1, Cs1, Fs1, Fs0, C0, Cs0) :-
	(	readws_whitespace_code( C1)
	->	Fs1 = [w*CsW|Fs2],
		readws_whitespace( C1, Cs1, CsW, Fs2, Fs0, C0, Cs0)
	;	readws_junk_code( C1)
	->	Fs1 = [j*CsJ|Fs2],
		readws_junk( C1, Cs1, CsJ, Fs2, Fs0, C0, Cs0)
	;	C1 = C0,
		Cs1 = Cs0,
		Fs1 = Fs0
	).

%------------------------------------------------------------------------------

% readws_lookup( ?Dict, +Name, ?Var) :-
%	this both adds named vars to, and retrieves named vars from,
%	the (open-ended list) Dict-ionary

readws_lookup( [N=V|L], Name, Var) :-
	(	N = Name
	->	V = Var
	;	readws_lookup( L, Name, Var)
	).

%------------------------------------------------------------------------------

% readws_name( -CsName, +C1, -C0, +Cs1, -Cs0) :-
%	reads a sequence of letters, digits, and underscores,
%	where the last character read was C1
%	and it is known that C1 is to be included in the result.
%	The consumed characters are returned as the list CsName,
%	and the remaining characters are [C0|Cs0].

readws_name( [C1|CsName1], C1, C0, [C2|Cs2], Cs0) :-
	(	C2 >= "a"
	->	(	C2 =< "z"			% ASCII lower case letter
		->	readws_name( CsName1, C2, C0, Cs2, Cs0)
		;	C2 < 192,			% {|}~, DEL, Latin-1 extended control and special
			C2 \/ 16 =\= 186	% except masculine (170) & feminine (186) ordinal indicators
		->	CsName1 = [],
			C2 = C0,
			Cs2 = Cs0
		;	C2 \/ 32 =:= 247	% multiplication sign (215) or division sign (247)
		->	CsName1 = [],
			C2 = C0,
			Cs2 = Cs0
		;	readws_name( CsName1, C2, C0, Cs2, Cs0)	% Latin-1 letters (upper and lower case)
		)
	;	C2 >= "A"
	->	(	C2 > "Z",
			C2 =\= "_"			% [\]^`
		->	CsName1 = [],
			C2 = C0,
			Cs2 = Cs0
		;	readws_name( CsName1, C2, C0, Cs2, Cs0)	% ASCII upper case or "_"
		)
	;	(	C2 >= "0",			% ASCII digits
			C2 =< "9"
		->	readws_name( CsName1, C2, C0, Cs2, Cs0)
		;	CsName1 = [],		% other ASCII characters
			C2 = C0,
			Cs2 = Cs0
		)
	).

%------------------------------------------------------------------------------

% readws_number( +C1, -Fs, -Fs1, ?Env, -Tokens, +Cs1) :-
%	C1 is the digit which begins the number

readws_number( C1, Fs, Fs1, E, [t(number,Errs,Number,Fs)|Ts1], Cs1) :-
	readws_number_1( CsSrcN1, CsN1, 0, N1, C1, C2, Cs1, Cs2),	% float(N1) -> integer overflow
	(	C2 =:= 0''								% this must be a based number; N1 is its base (represented by CsN1)
	->	(	N1 =:= 0							% special case: value of number is code of next character
		->	Cs2 = [CN2|Cs2a],					% cannot fail, although CN2 may be -1 i.e. EOF
			(	readws_char( N2, CsN2, [], Errs, -2, E, CN2, C0, Cs2a, Cs0)
			->	Number = N2
			;	C0 = CN2,						% resume tokenising from the unacceptable char
				Cs0 = Cs2a,
				Errs = [char_const(bad_char)],
				Number = -1,					% arbitrary value
				CsN2 = []
			)
		;	readws_based( CsN2, N1, 0, N, C0, Cs2, Cs0), % spurns digits inappropriate for the base
			(	N1 >= 2,
				N1 =< 36						% valid base 0..1 up to 0..9+A..Z (whatever that's called)
			->	(	CsN2 == []					% no digits after the quote
				->	Errs = [based(no_digits)],
					Number = -1					% arbitrary value
				;	Errs = [],
					Number = N					% as computed by readws_based
				)
			;	(	CsN2 == []					% no digits after quote; base bad (or overflowed)
				->	Errs = [based(bad_base),based(no_digits)],
					Number = -1					% arbitrary value
				;	Errs = [based(bad_base)],
					Number = -1
				)
			)
		),
		flatten( [CsSrcN1,C2,CsN2], CsNB),		% reconstruct sourcetext of based number (good or bad)
		Fs1 = [p*CsNB],
		Ts1 = Ts2
	;	C2 =:= 0'.								% this may be a float, or an integer followed by a fullstop
	->	Cs2 = [C2a|Cs2a],
		(	C2a >= "0",
			C2a =< "9"							% it must be a float
		->	readws_float( Float, Errs, CsFrac, CsN1, C2a, C0, Cs2a, Cs0),
			Number = Float,
			flatten( [CsSrcN1,C2,CsFrac], CsFloat),
			Fs1 = [p*CsFloat],
			Ts1 = Ts2
		;	Errs = [],							% it's an integer (followed by a fullstop presumably)
			Number = N1,
			Fs1 = [p*CsSrcN1],
			readws_fullstop( C2a, FsNew, FsNew, E, T, Cs2a, C0, Cs0),
			Ts1 = [T|Ts2]
		)
	;	% presume it's just an integer
		(	float( N1)
		->	Errs = [integer(overflow)]
		;	Errs = []
		),
		Number = N1,
		Fs1 = [p*CsSrcN1],
		C2 = C0,
		Cs2 = Cs0,
		Ts1 = Ts2
	),
	readws_tokens_1( C0, E, Ts2, Cs0).

%------------------------------------------------------------------------------

% readws_number_1( -CsSrc, -CsN, +N0, -N, +C1, -C0, +Cs1, -Cs0) :-
%	read (the rest of) a decimal integer (possibly containing layout (?) underscores)
%	whose value-so-far is N0
%	and whose net value is N (this is a float iff it has overflowed)
%	from [C1|Cs1]
%	giving CsSrc (the full sourcetext of the integer, including any underscores)
%	and CsN (a digits-only representation, for use e.g. in float evaluation)
%	leaving [C0|Cs0]

readws_number_1( CsSrc, CsN, N0, N, C1, C0, Cs1, Cs0) :-
	(	C1 >= "0",
		C1 =< "9"
	->	N1 is N0*10 - "0" + C1,			% may overflow (to float in SWIPL) 
		CsSrc = [C1|CsSrc1],
		CsN = [C1|CsN1],
		Cs1 = [C2|Cs2],
		readws_number_1( CsSrc1, CsN1, N1, N, C2, C0, Cs2, Cs0)
	;	C1 =:= 0'_
	->	CsSrc = [C1|CsSrc1],
		CsN = CsN1,
		Cs1 = [C2|Cs2],
		readws_number_1( CsSrc1, CsN1, N0, N, C2, C0, Cs2, Cs0)
	;	CsSrc = [],
		CsN = [],
		N0 = N,
		C1 = C0,
		Cs1 = Cs0
	).

%------------------------------------------------------------------------------

% readws_opt_inline_whitespace( +C1, +Cs1, -CsWhitespace, -C0, -Cs0) :-

readws_opt_inline_whitespace( C1, Cs1, CsWs, C0, Cs0) :-
	(	C1 =\= -1,			% not EOF
		C1 =< " ",			% layout: CR, LF, TAB, space, &c
		\+ readws_eol( C1)	% not end-of-line
	->	CsWs = [C1|CsWs2],
		Cs1 = [C2|Cs2],
		readws_opt_inline_whitespace( C2, Cs2, CsWs2, C0, Cs0)
	;	CsWs = [],
		C1 = C0,
		Cs1 = Cs0
	).

%------------------------------------------------------------------------------

% readws_post_eos( -Fs, ?Env, +C1, +Cs1, -C0, -Cs0) :-
%	consumes, and stashes in Fs, all contiguous whitespace and single-line comments,
%	and (if found) an end-of-line;
%	binds Env's status iff premature eof is found within solidus comment

readws_post_eos( Fs, E, C1, Cs1, C0, Cs0) :-
	readws_opt_inline_whitespace( C1, Cs1, CsWS, C2, Cs2),
	(	CsWS == []
	->	Fs = Fs2
	;	Fs = [w*CsWS|Fs2]
	),
	(	readws_inline_solidus( CsC, E, C2, Cs2, C2a, Cs2a)
	->	Fs2 = [c2*CsC|Fs3],
		readws_post_eos( Fs3, E, C2a, Cs2a, C0, Cs0)
	;	(	C2 =:= 0'%
		->	readws_comment( CsC, E, C2, C3, Cs2, Cs3),
			Fs2 = [c1*CsC|Fs3]
		;	Fs2 = Fs3,
			C2 = C3,
			Cs2 = Cs3
		),
		(	readws_eol( C3)
		->	Fs3 = [w*[C3]],
			Cs3 = [C0|Cs0]
		;	Fs3 = [],
			C3 = C0,
			Cs3 = Cs0
		)
	).

%------------------------------------------------------------------------------

readws_quote_context( 0'', quotes(single)).

readws_quote_context( 0'", quotes(double)).

readws_quote_context( -2, char_const).


%------------------------------------------------------------------------------

% readws_rest_string( -CsString, -CsSrc, -Errs, +Char, +Quote, ?Env, +C1, -C0, +Cs1, -Cs0) :-

readws_rest_string( [], [], [], -1, _, _, C, C, Cs, Cs) :-		% string ended (?)
	!.

readws_rest_string( [Char|CsString], CsSrc1, Errs, Char, Quote, E, C1, C0, Cs1, Cs0) :-
	readws_char( Char2, CsSrc1, CsSrc2, Errs1, Quote, E, C1, C2, Cs1, Cs2),
	readws_rest_string( CsString, CsSrc2, Errs2, Char2, Quote, E, C2, C0, Cs2, Cs0),
	append( Errs1, Errs2, Errs).

%------------------------------------------------------------------------------

% readws_rest_symbol( -CsRestSym, +C1, -C0, +Cs1, -Cs0) :-
%	reads any subsequent characters of an atom made up of
%	"symbol" characters.  It returns those characters as the list
%	CsRestSym, and the following characters are [C0|Cs0].
%	Note that it need not read any characters at all, e.g. C1 might be " ".

readws_rest_symbol( CsRestSym, C1, C0, Cs1, Cs0) :-
	(	(	C1 > 160		% Latin-1 special characters above non-breaking space
		->	C1 < 192,
			C1 =\= 186,		% masculine ordinal indicator
			C1 =\= 170		% feminine ordinal indicator
		;	readws_symbol_char( C1)
		)
	->	CsRestSym = [C1|CsRestSym1],
		Cs1 = [C2|Cs2],
		readws_rest_symbol( CsRestSym1, C2, C0, Cs2, Cs0)
	;	CsRestSym = [],
		C1 = C0,
		Cs1 = Cs0
	).

%------------------------------------------------------------------------------

% readws_scan_codes( +Cs1, -Cs0, -Status) :-
%	if Cs1 are each in 0..127, then Cs0 is Cs1 with -1 postpended and Status=7;
%	if Cs1 are each in 0..255 then Cs0 is Cs1 with -1 postpended and Status=8;
%	else Status=bad(N,C) where C is the first out-of-range code and N is its index
%	(numbering the codes from 1)

readws_scan_codes( Cs1, Cs0, Status) :-
	readws_scan_codes_1( Cs1, Cs0, 1, 7, Status).


%------------------------------------------------------------------------------

readws_scan_codes_1( [], [-1], _, S, S).

readws_scan_codes_1( [C|Cs1], [C|Cs0], N, S1, S0) :-
	(	(	C < 0
		;	C > 255
		)
	->	Cs1 = Cs0,
		S0 = bad_nth_code(N,C)
	;	(	C > 127
		->	8 = S2
		;	S1 = S2
		),
		N1 is N+1,
		readws_scan_codes_1( Cs1, Cs0, N1, S2, S0)
	).
 
		
%------------------------------------------------------------------------------

% readws_solidus( -CsComment, +N1, -N0, ?Env, +C1, -C0, +Cs1, -Cs0) :-
%	is called when we have read the "/" and "*" that open a comment.
%	CsComment is the sourcetext of the rest of the comment including
%	the first "*/" sequence (such comments do not nest) if any.
%	If the comment is terminated (prematurely) by an end-of-file marker,
%	Env's status variable is bound to eof(comment(c2) and the marker
%	is not included in CsComment but is returned as C0.
%	N1 is the qty of lines spanned by the comment so far (at least 1);
%	N0 is the qty of lines spanned by the entire comment.

readws_solidus( CsComment, N1, N0, E, C1, C0, Cs1, Cs0) :-
	(	C1 =:= 0'*							% possible end of comment*
	->	CsComment = [C1|CsComment2],
		Cs1 = [C2|Cs2],
		(	C2 =:= 0'/						% definite end of comment*/
		->	CsComment2 = [C2],
			N1 = N0,
			Cs2 = [C0|Cs0]
		;	readws_solidus( CsComment2, N1, N0, E, C2, C0, Cs2, Cs0)
		)
	;	C1 =:= -1							% (premature) EOF
	->	CsComment = [],						% exit with sourcetext-so-far
		N1 = N0,
		memberchk( status-eof(comment(c2)), E),
		C1 = C0,
		Cs1 = Cs0
	;	CsComment = [C1|CsComment2],		% ordinary comment character
		(	readws_eol( C1)
		->	N2 is N1+1						% count of lines spanned
		;	N2 is N1
		),
		Cs1 = [C2|Cs2],						% look for more
		readws_solidus( CsComment2, N2, N0, E, C2, C0, Cs2, Cs0)
	).

%------------------------------------------------------------------------------

% readws_special( +C1, -Fs, -Fs1, ?Env, -Tokens, +Cs1) :-

readws_special( 0'_, Fs, Fs1, E, Ts, Cs1) :-				% underscore; starts var
	readws_variable( 0'_, Fs, Fs1, E, Ts, Cs1).

readws_special( 247, Fs, Fs1, E, Ts, Cs1) :-				% Latin-1 division sign
	readws_symbol( 247, Fs, Fs1, E, Ts, Cs1).

readws_special( 215, Fs, Fs1, E, Ts, Cs1) :-				% Latin-1 multiplication sign
	readws_symbol( 215, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'%, Fs, [c1*CsComment|Fs2], E, Ts, Cs1) :-	%  %comment
	readws_comment( CsComment, E, 0'%, C2, Cs1, Cs2),
	readws_tokens_2( C2, Fs, Fs2, E, Ts, Cs2).

readws_special( 0'/, Fs, Fs1, E, Ts, [C2|Cs2]) :-				% possibly a /*comment
	(	C2 =:= "*"												% definitely a /*comment
	->	Cs2 = [C3|Cs3],
		readws_solidus( CsComment, 1, _N, E, C3, C4, Cs3, Cs4),
		Fs1 = [c2*[0'/,0'*|CsComment]|Fs2],
		readws_tokens_2( C4, Fs, Fs2, E, Ts, Cs4)
	;	readws_rest_symbol( CsRestSymbol, C2, C3, Cs2, Cs3),	% C2 continues symbol
		CsSymbol = [0'/|CsRestSymbol],
		atom_codes( Symbol, CsSymbol),
		Fs1 = [p*CsSymbol],
		Ts = [t(atom,[],Symbol,Fs)|Ts2],
		readws_tokens_1( C3, E, Ts2, Cs3)
	).

readws_special( 0'!, Fs, [p*"!"], E, [t(atom,[],!,Fs)|Ts], [C2|Cs2]) :-	% special case so that "!." is two tokens (it could be cleverer)
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special( 0'(, Fs, [s*"("], E, [t('(',[],'(',Fs)|Ts], [C2|Cs2]) :-
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0'), Fs, [s*")"], E, [t(')',[],')',Fs)|Ts], [C2|Cs2]) :-
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0',, Fs, [s*","], E, [t(',',[],',',Fs)|Ts], [C2|Cs2]) :-
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0';, Fs, [s*";"], E, [t(atom,[],';',Fs)|Ts], [C2|Cs2]) :-	% ; is atom, not punctuation (e.g. you can :-op declare it)
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0'[, Fs, [s*"["], E, [t('[',[],'[',Fs)|Ts], [C2|Cs2]) :-
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0'], Fs, [s*"]"], E, [t(']',[],']',Fs)|Ts], [C2|Cs2]) :-
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0'{, Fs, [s*"{"], E, [t('{',[],'{',Fs)|Ts], [C2|Cs2]) :-
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0'|, Fs, [s*"|"], E, [t('|',[],'|',Fs)|Ts], [C2|Cs2]) :-
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0'}, Fs, [s*"}"], E, [t('}',[],'}',Fs)|Ts], [C2|Cs2]) :-
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special(0'., Fs, Fs1, E, [T|Ts1], [C2|Cs2]) :-		% full stop or possibly .=. &c
	readws_fullstop( C2, Fs, Fs1, E, T, Cs2, C3, Cs3),
	readws_tokens_1( C3, E, Ts1, Cs3).

readws_special( 0'", Fs, [p*[0'"|CsSrc]], E, [t(string,Errs,CsStringBody,Fs)|Ts], Cs1) :-
	readws_string( CsStringBody, CsSrc, Errs, 0'", E, C2, Cs1, Cs2),			% "string"
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special( 0'', Fs, [p*[0''|CsSrc]], E, [t(atom,Errs,AtomBody,Fs)|Ts], Cs1) :-
	readws_string( CsAtomBody, CsSrc, Errs, 0'', E, C2, Cs1, Cs2),			%  'atom'
	atom_codes( AtomBody, CsAtomBody),
	readws_tokens_1( C2, E, Ts, Cs2).

readws_special( 0'#, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'#, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'$, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'$, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'&, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'&, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'*, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'*, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'+, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'+, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'-, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'-, Fs, Fs1, E, Ts, Cs1).

readws_special( 0':, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0':, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'<, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'<, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'=, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'=, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'>, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'>, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'?, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'?, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'@, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'@, Fs, Fs1, E, Ts, Cs1).

readws_special( 92, Fs, Fs1, E, Ts, Cs1) :-		% 92 is backslash
	readws_symbol( 92, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'^, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'^, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'`, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'`, Fs, Fs1, E, Ts, Cs1).

readws_special( 0'~, Fs, Fs1, E, Ts, Cs1) :-
	readws_symbol( 0'~, Fs, Fs1, E, Ts, Cs1).

%------------------------------------------------------------------------------

% readws_string( -CsString, -CsSrc, -Errs, +Quote, ?Env, -C0, +Cs1, -Cs0) :-
%	reads the body of a string delimited by Quote (single or double) characters.
%	CsString is a list of ASCII or Latin-1 codes of the characters
%	which the string represents (after e.g. character escapes are interpreted).
%	Errs = [eof] if we hit the end of the file inside the string, else [].
%	A complication is that when we find a Quote
%	we have to look ahead one character in case it is doubled.  Note that
%	if we find an end-of-file after the quote we *don't* fail, we return
%	a normal string and the end of file character is returned as NextCh.
%	If we were going to accept C-like escape characters, as I think we
%	should, this would need changing (as would the code for 0'x).  But
%	the purpose of this module is not to present my ideal syntax but to
%	present something which will read present-day Prolog programs.

readws_string( CsString, CsSrc1, Errs, Quote, E, C0, [C2|Cs2], Cs0) :-
	readws_char( Char, CsSrc1, CsSrc2, Errs1, Quote, E, C2, C3, Cs2, Cs3),
	readws_rest_string( CsString, CsSrc2, Errs2, Char, Quote, E, C3, C0, Cs3, Cs0),
	append( Errs1, Errs2, Errs).

%------------------------------------------------------------------------------

% readws_symbol( +C1, -Fs, -Fs1, ?Env, -Tokens, +Cs1) :-
%	C1 encodes the first char of an atom made up of the following chars:
%	ASCII: # $ & * + - . / : < = > ? \ ^ ' ~
%	Latin-1: 161-169, 171-185, 187-191, 215 (multiplication), 247 (division)

readws_symbol( C1, Fs, [p*CsSymbol], E, [t(atom,[],Symbol,Fs)|Ts], [C2|Cs2]) :-
	readws_rest_symbol( CsRestSymbol, C2, C3, Cs2, Cs3),	% might read 0 codes
	CsSymbol = [C1|CsRestSymbol],
	atom_codes( Symbol, CsSymbol),
	readws_tokens_1( C3, E, Ts, Cs3).

%------------------------------------------------------------------------------

readws_symbol_char( 0'#).
readws_symbol_char( 0'$).
readws_symbol_char( 0'&).
readws_symbol_char( 0'*).
readws_symbol_char( 0'+).
readws_symbol_char( 0'-).
readws_symbol_char( 0'.).	% yes, +./* is a legal atom (but it needs quoting)
readws_symbol_char( 0'/).
readws_symbol_char( 0':).
readws_symbol_char( 0'<).
readws_symbol_char( 0'=).
readws_symbol_char( 0'>).
readws_symbol_char( 0'?).
readws_symbol_char( 0'@).
readws_symbol_char( 92).	% 92 encodes backslash
readws_symbol_char( 0'^).
readws_symbol_char( 0'`).	% CHAT-80 uses `` as an atom.
readws_symbol_char( 0'~).

%------------------------------------------------------------------------------

% readws_terminate_list( ?Terms) :-
%	unifies an improper list with the shortest matching proper list
%	(i.e. sticks a [] on its tail if it hasn't already got one)

readws_terminate_list( Xs) :-
	(	Xs = []
	->	true
	;	Xs = [_|Xs2]
	->	readws_terminate_list( Xs2)
	).

%------------------------------------------------------------------------------

% readws_tokens_1( +C, ?Env, -Tokens, +Cs) :-
%	(re)start with no (Fs-Fs) accumulated sourcetext fragments
%	this procedure could be inlined...

readws_tokens_1( C, E, Ts, Cs) :-
	readws_tokens_2( C, Fs, Fs, E, Ts, Cs).

%------------------------------------------------------------------------------

% readws_tokens_2( +C1, -Fs, -Fs1, ?Env, -Tokens, +Cs1) :-
%	C1 encodes the next character to be considered (now);
%	Fs-Fs1 is a list of sourcetext fragments, each is one of
%	{ p*Cs | s*Cs | t*Cs | c1*Cs | c2*Cs | w*Cs | j*Cs }

readws_tokens_2( C1, Fs, Fs1, E, Ts, Cs1) :-
	(	C1 =:= -1			% end-of-file marker
	->	Fs1 = [],
		Ts = [t(eof,[],eof,Fs)]
	;	C1 =< 32			% ASCII control or space
	->	readws_layout( C1, Cs1, Fs1, Fs2, C2, Cs2),
		readws_tokens_2( C2, Fs, Fs2, E, Ts, Cs2)
	;	C1 >= "a",			% ASCII lowercase letter
		C1 =< "z"
	->	readws_identifier( C1, Fs, Fs1, E, Ts, Cs1)
	;	C1 >= "A",			% ASCII uppercase letter
		C1 =< "Z"
	->	readws_variable( C1, Fs, Fs1, E, Ts, Cs1)
	;	C1 >= "0",			% ASCII decimal digit
		C1 =< "9"
	->	readws_number( C1, Fs, Fs1, E, Ts, Cs1)
	;	C1 < 127			% other ASCII printable character
	->	readws_special( C1, Fs, Fs1, E, Ts, Cs1)
	;	C1 =< 160			% ASCII DEL or Latin-1 extended control
	->	readws_layout( C1, Cs1, Fs1, Fs2, C2, Cs2),
		readws_tokens_2( C2, Fs, Fs2, E, Ts, Cs2)
	;	C1 >= 223,			% Latin-1 lower case letter
		C1 =\= 247			% excluding division sign
	->	readws_identifier( C1, Fs, Fs1, E, Ts, Cs1)
	;	C1 >= 192,			% Latin-1 upper case letter
		C1 =\= 215			% excluding multiplication sign
	->	readws_variable( C1, Fs, Fs1, E, Ts, Cs1)
	;						% Latin-1 special character other than...
		C1 =\= 170,			% feminine ordinal indicator
		C1 =\= 186			% masculine ordinal indicator
	->	readws_symbol( C1, Fs, Fs1, E, Ts, Cs1)
	;						% masculine or feminine ordinal indicator
		readws_identifier( C1, Fs, Fs1, E, Ts, Cs1)
	).

%------------------------------------------------------------------------------

% readws_tokens_to_pltext( +Tokens, -PlText) :-
%	a PlText is either a q/4 structure or an error/N structure

readws_tokens_to_pltext( Ks, P) :-
	stp_parse( Ks, _T, P).

  %	(	K = t(Class,_,_,_),
  %		stp_legacy_ops( E),
  %		stp_parse_2( Class, [K|Ks], 1200, E, Q, Ks2),
  %		(	Ks2 = [t(eos,_,'.',Fs)]
  %		->	Q = q(C,T,Qs,Fss1),
  %			append( Fss1a, [Fs1z], Fss1),
  %			append( Fs1z, Fs, Fs2z),
  %			append( Fss1a, [Fs2z], Fss2),
  %			P = q(C,T,Qs,Fss2)
  %		;	stp_put_stp_syntax_error(
  %				[operator,expected,after,expression],
  %				[K|Ks]
  %			)
  %		)
  %	;	stp_get_stp_syntax_error( [K|Ks], Err),
  %		P = Err									% NB no further instantiation of Term
  %	).

%------------------------------------------------------------------------------

% readws_tokens_to_sentence_tokenss( +Tokens, -Tokenss) :-
%	partitions Tokens into the list-of-list-of-token Tokenss
%	such that each t(eos,_,_,_) ends a member list of Tokenss;
%	NB the last such list oughta but might not have a final eos token

readws_tokens_to_sentence_tokenss( [t(eof,_,eof,_)], []) :-
	!.

readws_tokens_to_sentence_tokenss( [T|Ts], Tss) :-
	readws_tokens_to_sentence_tokenss_1( [T|Ts], TsNew, TsNew, Tss).

%------------------------------------------------------------------------------

% readws_tokens_to_sentence_tokenss_1( +Ts, -TsH, -TsT, -Tss) :-

readws_tokens_to_sentence_tokenss_1( [T|Ts], TsH, TsT, Tss) :-
	(	T = t(eos,_,_,_)										% end of this sentence
	->	TsT = [T],
		Tss = [TsH|Tss2],
		readws_tokens_to_sentence_tokenss( Ts, Tss2)			% maybe start another
	;	T = t(eof,_,_,_)										% premature end-of-file
	->	TsT = [],												% ends this (non)sentence
		Tss = [TsH|Tss2],
		readws_tokens_to_sentence_tokenss( [T|Ts], Tss2)		% restart from eof
	;	TsT = [T|TsT2],											% another token in this sentence
		readws_tokens_to_sentence_tokenss_1( Ts, TsH, TsT2, Tss)
	).

%------------------------------------------------------------------------------

% readws_variable( +C1, -Fs, -Fs1, ?Env, -Tokens, +Cs1) :-
%	C1 encodes the first character of a variable name.  If the whole
%	variable name is "_", this is an anonymous variable, not identical
%	to any other variable.  Otherwise, the variable and its name are
%	looked up in (or added to) the dictionary, which is an improper list.
%	This is the only place that readws_lookup/2 is called.

readws_variable( C1, Fs, Fs1, E, [t(var(Name),[],Var,Fs)|Ts], Cs1) :-
	readws_name( CsName, C1, C2, Cs1, Cs2),
	atom_codes( Name, CsName),
	Fs1 = [p*CsName],
	(	Name == '_'
	->	true
	;	memberchk( dict-Dict, E),
		readws_lookup( Dict, Name, Var)
	),
	readws_tokens_1( C2, E, Ts, Cs2).

%------------------------------------------------------------------------------

% readws_whitespace( +CWhiteSpace, +Cs1, -CsWhiteSpace, -Fs1, +Fs0, -C0, -Cs0) :-

readws_whitespace( CW, [C1|Cs1], [CW|CsW], Fs1, Fs0, C0, Cs0) :-
	(	readws_whitespace_code( C1)
	->	readws_whitespace( C1, Cs1, CsW, Fs1, Fs0, C0, Cs0)
	;	CsW = [],
		readws_layout( C1, Cs1, Fs1, Fs0, C0, Cs0)
	).

%------------------------------------------------------------------------------

% readws_whitespace_code( -Code) :-
%	Code encodes a whitespace character
%	(should this include Latin-1 non-breaking space?)

readws_whitespace_code(  9).	% TAB
readws_whitespace_code( 10).	% LF
readws_whitespace_code( 13).	% CR
readws_whitespace_code( 32).	% space

%------------------------------------------------------------------------------

% curin_to_chars( -Bytes) :-
%	Bytes becomes instantiated to a list of integer byte values
%	from the file opened by file_chars/2.
%	Paul Singleton, Oct 1987 (he's proud of this one).
%	(also, he's got eleven slower versions if you are interested.)
%	Wasn't originally clear whether a QP 'char' was 0..127 or 0..255

curin_to_chars( Cs) :-
	get0( C),
	(	C == -1
	->	Cs = []
	;	Cs = [C|Cs1],
		curin_to_chars( Cs1)
	).

%------------------------------------------------------------------------------

% file_to_chars_v5( +Pathatom, -Chars) :-
%	Chars is a list of the integer values of the bytes
%	in the UNIX file named by Pathatom.
%	If the file is a textfile, then Chars is a 'chars'
%	(maybe it is anyway)

file_to_chars_v5( Path, Chars) :-
	open( Path, read, S),
	current_input( Input),
	set_input( S),
	(	curin_to_chars( Chars)
	->	close( S),
		set_input( Input)
	;	close( S),
		set_input( Input),
		fail
	).

%------------------------------------------------------------------------------

% stp_ambigop( +Symbol, +Precedence, +Env, -L1, -O1, -R1, -L2, -O2) :-
%	is true when Symbol has an infix (L1,O1,R1) and a postfix (L2,O2)
%	definition both of which are compatible with Precedence.
%	I assume here that postfixop and infixop have been coded so that
%	they are determinate when called with a constant first argument.

stp_ambigop( F, Precedence, E, L1, O1, R1, L2, O2) :-
	stp_postfixop( F, L2, O2, E),
	O2 =< Precedence,
	stp_infixop( F, L1, O1, R1, E),
	O1 =< Precedence.

%------------------------------------------------------------------------------

% stp_cannot_follow( +Type, +Token, +Tokens) :-

stp_cannot_follow( Type, Token, Tokens) :-
	stp_put_stp_syntax_error( [Type,follows,expression], [Token|Tokens]).

%------------------------------------------------------------------------------

% stp_cannot_start( +Token, +Tokens) :-

stp_cannot_start( Token, S0) :-
	stp_put_stp_syntax_error( [Token,cannot,start,an,expression], S0).

%------------------------------------------------------------------------------

% stp_expect( +Class, +TokensIn, -TokensOut) :-
%	reads the next token,
%	checks that it is the one expected,
%	and gives an error message if it is not.
%	It is used to look for right brackets of various sorts,
%	as they're all we can be sure of.

stp_expect( Class, [t(Class,_,_,_)|Ks], Ks) :-
	!.

stp_expect( Class, Ks0, _) :-
	stp_put_stp_syntax_error( [Class,or,operator,expected], Ks0).

%------------------------------------------------------------------------------

% stp_exprtl( +Tokens0, +C, +Term1, +Q1, +Prec, +Env, -Q, -TokensOut) :-
%	is called by after_prefix_op or exprtl0 when the first token of
%	S0 has already been peeked at.  Basically, it parses	
%	{ <infixop> <operand> | <postfixop> }*

stp_exprtl( [], _, _, Q, _, _, Q, []).

stp_exprtl( [K|Ks1], C, T1, Q1, P, E, Q, Ks) :-
	stp_exprtl_1( K, C, T1, Q1, P, E, Q, Ks, Ks1).

%------------------------------------------------------------------------------

% stp_exprtl0( +TokensIn, +Term1, +Q1, +Prec, +Env, -Q, -TokensOut) :-
%	is called by parse/4 after it has read a primary (the Term) which
%	has not caused the next token to be peeked at.  That is, the
%	Term was a constant, a variable, a plain compound term, or was
%	bracketed with () or [] or {}.  An atom which might have been an
%	operator or a term built up out of operators will result in a call
%	to exprtl/6 instead.
%	It checks for following postfix or infix operators.

stp_exprtl0( [], _, Q, _, _, Q, []).

stp_exprtl0( [K|Ks1], T1, Q1, Precedence, E, Q, Ks) :-
	K = t(Class,_,_,_),
	stp_exprtl0_1( Class, K, T1, Q1, Precedence, E, Q, Ks, Ks1).

%------------------------------------------------------------------------------

% stp_exprtl0_1( +Class, +Token, +Term1, +Q1, +Prec, +Env, -Q, -TokensOut, +TokensIn) :-

stp_exprtl0_1( eos, K, _, Q, _, _, Q, [K|Ks1], Ks1).	% push token back

stp_exprtl0_1( '}', K, _, Q, _, _, Q, [K|Ks1], Ks1).	% push token back

stp_exprtl0_1( ']', K, _, Q, _, _, Q, [K|Ks1], Ks1).	% push token back

stp_exprtl0_1( ')', K, _, Q, _, _, Q, [K|Ks1], Ks1).	% push token back

stp_exprtl0_1( ',', K, T1, Q1, P, E, Q, Ks, Ks1) :-
	(	P >= 1000
	->	stp_parse_1( Ks1, 1000, E, Q2, Ks2),
		!,
		K = t(',',_,_,Fs),							% get comma's text
		Q2 = q(_,T2,_,_),							% extract the 2nd arg of ,/2
		T3 = (T1,T2),
		Q3 = q(c,T3,[Q1,Q2],[[],Fs,[]]),		% put between arg texts
		stp_exprtl( Ks2, 1000, T3, Q3, P, E, Q, Ks)
	;	Q = Q1,
		Ks = [K|Ks1]
	).

stp_exprtl0_1( '|', K, T1, Q1, P, E, Q, Ks, Ks1) :-		% synonym for ';'
	(	P >= 1100
	->	stp_parse_1( Ks1, 1100, E, Q2, Ks2),
		!,
		K = t('|',_,_,Fs),
		Q2 = q(_,T2,_,_),
		T3 = (T1;T2),
		Q3 = q(c,T3,[Q1,Q2],[[],Fs,[]]),
		stp_exprtl( Ks2, 1100, T3, Q3, P, E, Q, Ks)
	;	Q = Q1,
		Ks = [K|Ks1]
	).

stp_exprtl0_1( string, K, _, _, _, _, _, _, Ks1) :-
	stp_cannot_follow( chars, K, Ks1).

stp_exprtl0_1( number, K, _, _, _, _, _, _, Ks1) :-
	stp_cannot_follow( number, K, Ks1).

stp_exprtl0_1( '{', t('{',_,_,Fs1), T1, Q1, P, E, Q, Ks, [t('}',_,_,Fs2)|Ks1]) :-
	!,
	append( Fs1, Fs2, Fs),
	stp_exprtl0_atom( '{}', Fs, T1, Q1, P, E, Q, Ks, Ks1).

stp_exprtl0_1( '{', K, _, _, _, _, _, _, Ks1) :-
	stp_cannot_follow( brace, K, Ks1).

stp_exprtl0_1( '[', t('[',_,_,Fs1), T1, Q1, P, E, Q, Ks, [t(']',_,_,Fs2)|Ks1]) :-
	!,
	append( Fs1, Fs2, Fs),
	stp_exprtl0_atom( '[]', Fs, T1, Q1, P, E, Q, Ks, Ks1).

stp_exprtl0_1( '[', K, _, _, _, _, _, _, Ks1) :-
	stp_cannot_follow( bracket, K, Ks1).

stp_exprtl0_1( '(', K, _, _, _, _, _, _, Ks1) :-
	stp_cannot_follow( parenthesis, K, Ks1).

stp_exprtl0_1( var(_), K, _, _, _, _, _, _, Ks1) :-
	stp_cannot_follow( variable, K, Ks1).

stp_exprtl0_1( atom, t(atom,_,A,FsA), T1, Q1, P, E, Q, Ks, Ks1) :-
	stp_exprtl0_atom( A, FsA, T1, Q1, P, E, Q, Ks, Ks1).

%------------------------------------------------------------------------------

% stp_exprtl0_atom( +Atom, +Fragments, +Term1, +Q1, +Prec, +Env, -Q, -TokensOut, +TokensIn) :-

stp_exprtl0_atom( A, FsA, T1, Q1, P, E, Q, Ks, Ks1) :-
	stp_ambigop( A, P, E, L1, O1, R1, L2, O2),
	!,
	(   stp_prefix_is_atom( Ks1, P),
	    !,
	    stp_exprtl( [t(postfixop(L2,O2),_,A,FsA) |Ks1], 0, T1, Q1, P, E, Q, Ks)
	;   stp_exprtl( [t(infixop(L1,O1,R1),_,A,FsA)|Ks1], 0, T1, Q1, P, E, Q, Ks)
	;   stp_exprtl( [t(postfixop(L2,O2),_,A,FsA) |Ks1], 0, T1, Q1, P, E, Q, Ks)
	).

stp_exprtl0_atom( A, FsA, T1, Q1, P, E, Q, Ks, Ks1) :-
	stp_infixop( A, L1, O1, R1, E),
	!,
	stp_exprtl( [t(infixop(L1,O1,R1),_,A,FsA)|Ks1], 0, T1, Q1, P, E, Q, Ks).

stp_exprtl0_atom( A, FsA, T1, Q1, P, E, Q, Ks, Ks1) :-
	stp_postfixop( A, L2, O2, E),
	!,
	stp_exprtl( [t(postfixop(L2,O2),_,A,FsA)|Ks1], 0, T1, Q1, P, E, Q, Ks).

stp_exprtl0_atom( X, FsX, _, _, _, _, _, _, Ks1) :-
	!,
	stp_put_stp_syntax_error( [non-operator,X,follows,expression], [t(atom,_,X,FsX)|Ks1]).

%------------------------------------------------------------------------------

% stp_exprtl_1( +Token, +C, +Term1, +Q1, +Prec, +Env, -Q, -TokensOut, +TokensIn) :-
%	is called by after_prefix_op or exprtl0
%	when the first token of Token has already been peeked at.
%	Basically, it parses { <infixop> <operand> | <postfixop> }*
%	NB strange -TokensOut, +TokensIn arg order...

stp_exprtl_1( t(infixop(L,O,R),_,A,Fs), C, T1, Q1, P, E, Q, Ks, Ks1) :-
	P >= O,
	C =< L,
	!,
	stp_parse_1( Ks1, R, E, Q2, Ks2),
	Q2 = q(_,T2,_,_),
	T3 =.. [A,T1,T2], 									% !,
	Q3 = q(c,T3,[Q1,Q2],[[],Fs,[]]),
	stp_exprtl( Ks2, O, T3, Q3, P, E, Q, Ks).

stp_exprtl_1( t(postfixop(L,O),_,A,Fs), C, T1, Q1, P, E, Q, Ks, Ks1) :-
	P >= O,
	C =< L,
	!,
	T2 =.. [A,T1],
	Q2 = q(c,T2,[Q1],[[],Fs]),
	stp_peepop( Ks1, E, Ks2),
	stp_exprtl( Ks2, O, T2, Q2, P, E, Q, Ks).

stp_exprtl_1( t(',',_,_,Fs), C, T1, Q1, P, E, Q, Ks, Ks1) :-
	P >= 1000,
	C < 1000,
	!,
	stp_parse_1( Ks1, 1000, E, Q2, Ks2),					% !,
	Q2 = q(_,T2,_,_),
	T3 = (T1,T2),
	Q3 = q(c,T3,[Q1,Q2],[[],Fs,[]]),
	stp_exprtl( Ks2, 1000, T3, Q3, P, E, Q, Ks).

stp_exprtl_1( t('|',_,_,Fs), C, T1, Q1, P, E, Q, Ks, Ks1) :-
	P >= 1100,
	C < 1100,
	!,
	stp_parse_1( Ks1, 1100, E, Q2, Ks2),
	Q2 = q(_,T2,_,_),
	T3 = (T1;T2),										% !,
	Q3 = q(c,T3,[Q1,Q2],[[],Fs,[]]),
	stp_exprtl( Ks2, 1100, T3, Q3, P, E, Q, Ks).

stp_exprtl_1( K, _, _, Q, _, _, Q, [K|Ks], Ks).

%------------------------------------------------------------------------------

% stp_get_stp_syntax_error( +Tokens, -Err) :-
%   This business of syntax errors is tricky.  When an error is detected,
%   we have to write out a message.  We also have to note how far it was
%   to the end of the input, and for this we are obliged to use the data-
%   base.  Then we fail all the way back to parse(), and that prints the
%   input TokensRemaining with a marker where the error was noticed.
%
%	If subgoal_of were available in compiled code we could use that to
%	find the input TokensRemaining without hacking the data base.
%
%	The really hairy thing is that
%   the original code noted a possible error and backtracked on, so that
%   what looked at first sight like an error sometimes turned out to be
%   a wrong decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no backtracking
%   at all.  This goal has not yet been met, and it will still occasionally
%   report an error message and then decide that it is happy with the input
%   after all.  Sorry about that.

stp_get_stp_syntax_error( Tokens, err(Message,Tokens,BeforeError)) :-
	recorded( syntax_error, Message-AfterError, Ref),
	erase( Ref),
	length( Tokens, Length),
	BeforeError is Length-AfterError.

%------------------------------------------------------------------------------

stp_infixop( F, P, O, Q, E) :-
  %	(	current_op( O, xfy, F)
  %	->	P is O-1,
  %		Q is O
  %	;	current_op( O, xfx, F)
  %	->	P is O-1,
  %		Q is P
  %	;	current_op( O, yfx, F)
  %	->	Q is O-1,
  %		P is O
  %	).
	memberchk( infixop(F,P,O,Q), E).

%------------------------------------------------------------------------------

% stp_legacy_op( -FixOp) :-
%	FixOp represents, in an obsolete way, a current operator definition
%	(collect the entire set!)

stp_legacy_op( FixOp) :-
	current_op( O, A, F),
	stp_legacy_op_1( A, O, F, FixOp).

%------------------------------------------------------------------------------

stp_legacy_op_1( xfy, O, F, infixop(F,Ob,O,O)) :-
	Ob is O-1.

stp_legacy_op_1( xfx, O, F, infixop(F,Ob,O,Ob)) :-
	Ob is O-1.

stp_legacy_op_1( yfx, O, F, infixop(F,O,O,Ob)) :-
	Ob is O-1.

stp_legacy_op_1( xf, O, F, postfixop(F,Ob,O)) :-
	Ob is O-1.

stp_legacy_op_1( yf, O, F, postfixop(F,O,O)).

stp_legacy_op_1( fx, O, F, prefixop(F,O,Ob)) :-
	Ob is O-1.

stp_legacy_op_1( fy, O, F, prefixop(F,O,O)).

%------------------------------------------------------------------------------

% stp_legacy_ops( -FixOps) :-

stp_legacy_ops( FixOps) :-
	findall(
		FixOp,
		stp_legacy_op( FixOp),
		FixOps
	).

%------------------------------------------------------------------------------

% stp_number_to_sourcetext_node_type( +Number, -SourcetextNodeType) :-
%	Number is an integer or float,
%	SourcetextNodeType is its type as used in first arg of q/4 

stp_number_to_sourcetext_node_type( N, Tsn) :-
	(	float( N)
	->	Tsn = f
	;	integer( N)
	->	Tsn = i
	).

%------------------------------------------------------------------------------

% stp_parse( +Tokens, -Term) :-
% stp_parse( +Tokens, -Term, -QtreeOrErr) :-
% stp_parse( +Tokens, +OpDefs, -Term, -QtreeOrErr) :-
%	recently fixed:
%	  *	return syntax error as a structure
%	  *	delete the redundant T return parameters (it's arg 2 of q/4)

stp_parse( Ks, Term) :-
	stp_parse( Ks, Term, q(_,_,_,_)).		% fail silently on syntax error


stp_parse( Ks1, Term, PlText) :-
	stp_legacy_ops( Ops),
	stp_parse( Ks1, Ops, Term, PlText).


stp_parse( Ks1, Ops, Term, PlText) :-
	(	stp_parse_1( Ks1, 1200, Ops, Q, Ks2)	% commit to first exit (?)
	->	(	Ks2 = [t(eos,_,'.',Fs)]				% the usual case
		->	Q = q(C,Term,Qs,Fss1),
			append( Fss1a, [Fs1z], Fss1),
			append( Fs1z, Fs, Fs2z),			% add the fullstop's sourcetext (inc post eos layout & comments)
			append( Fss1a, [Fs2z], Fss2),
			PlText = q(C,Term,Qs,Fss2)
		;	stp_put_stp_syntax_error( 
				[operator,expected,after,expression],
				Ks2
			)							% (fails)
		)
	;	stp_get_stp_syntax_error( Ks1, Err),
		PlText = Err					% NB no further instantiation of Term
	).

%------------------------------------------------------------------------------

% stp_parse_1( +TokensIn, +Precedence, +Env, -Q, -TokensOut)
%	parses TokensIn in a context of Precedence,
%	returning a term within its textual structure Q
%	and the unread TokensOut
%
%	There are several occurrences of parse/N in the context
%	stp_parse_1(S1, Prio, Term, S2),
%	expect_one_of(PunctuationMarks, S2)
%	!
%	The punctuation marks in question are
%	, | ]			[lists]
%	}			{goals}
%	, )			compound(terms)
%	)			(parenthesised,terms)
%	-|			complete term; no actual token, alas.
%	Now the point here is that if we do not find the right kind of token,
%	we'll store an error message and then try to provoke parse/4 into
%	finding another parse which does end at the right spot.  We do NOT
%	like having cuts all over the place.  So to try to make things clearer,
%	we have a predicate full_parse/4 which goes as far as it possibly can.
%
%	K is a token
%	Ks is a list of tokens
%	P is a precedence
%	T is a term constructed from parsed tokens
%	Q is a textual structure of an associated T

stp_parse_1( [], _, _, _, _) :-
	stp_put_stp_syntax_error( [expression,expected], []).

stp_parse_1( [K|Ks1], P, E, Q, Ks) :-
	K = t(Class,_,_,_),
	stp_parse_2( Class, [K|Ks1], P, E, Q, Ks).

%------------------------------------------------------------------------------

% stp_parse_2( +Class, +TokensIn, +Precedence, +Env, -Q, -TokensOut) :-

stp_parse_2( '}', [_|Ks0], _, _, _, _) :-
	stp_cannot_start( '}', Ks0).

stp_parse_2( ']', [_|Ks0], _, _, _, _) :-
	stp_cannot_start( ']', Ks0).

stp_parse_2( ')', [_|Ks0], _, _, _, _) :-
	stp_cannot_start( ')', Ks0).

stp_parse_2( ',', [_|Ks0], _, _, _, _) :-
	stp_cannot_start( ',', Ks0).

stp_parse_2( '|', [_|Ks0], _, _, _, _) :-
	stp_cannot_start( '|', Ks0).

stp_parse_2( string, [t(string,_,Cs,Fs)|Ks0], P, E, Q, Ks) :-
	T1 = Cs,
	Q1 = q(s,Cs,[],[Fs]),
	stp_exprtl0( Ks0, T1, Q1, P, E, Q, Ks).

stp_parse_2( number, [t(number,_,N,Fs)|Ks0], P, E, Q, Ks) :-
	stp_number_to_sourcetext_node_type( N, Tsn),
	T1 = N,
	Q1 = q(Tsn,T1,[],[Fs]),
	stp_exprtl0( Ks0, T1, Q1, P, E, Q, Ks).

stp_parse_2( '[', [t('[',_,_,Fs1),t(']',_,_,Fs2)|Ks1], P, E, Q, Ks) :-
	!,
	append( Fs1, Fs2, Fs),								% combine fragments
	stp_read_atom( '[]', Fs, Ks1, P, E, Q, Ks).

stp_parse_2( '[', [t('[',_,_,Fs1)|Ks1], P, E, Q, Ks) :-
	stp_parse_1( Ks1, 999, E, Qa, Ks2),					% look for ",", "|", or "]"
	Qa = q(_,Arg,_,_),
	stp_read_list( Ks2, E, Args, Qas, Fs2, Fs3, Ks3),
	!,
	T1 = [Arg|Args],
	Q1 = q(c,T1,[Qa,Qas],[Fs1,Fs2,Fs3]),
	stp_exprtl0( Ks3, T1, Q1, P, E, Q, Ks).

stp_parse_2( '(', [t('(',_,_,Fs1)|Ks1], P, E, Q, Ks) :-
	stp_parse_1( Ks1, 1200, E, Q1, Ks2),					% look for ")"
	Q1 = q(_,T1,_,_),
	stp_expect( ')', Ks2, Ks3),
	!,
	Ks2 = [t(_,_,_,Fs2)|_],
	Q1 = q(Tsn,T1,Qs,Fss),
	Fss = [FsPre|Fss2],
	append( Fs1, FsPre, FsPre2),
	Fss3 = [FsPre2|Fss2],
	append( Fss4, [FsPost], Fss3),
	append( FsPost, Fs2, FsPost2),
	append( Fss4, [FsPost2], Fss0),
	Q1a = q(Tsn,T1,Qs,Fss0),
	stp_exprtl0( Ks3, T1, Q1a, P, E, Q, Ks).

stp_parse_2( '{', [t('{',_,_,Fs1),t('}',_,_,Fs2)|Ks1], P, E, Q, Ks) :-
	!,
	append( Fs1, Fs2, Fs),
	stp_read_atom( '{}', Fs, Ks1, P, E, Q, Ks).

stp_parse_2( '{', [t('{',_,_,Fs1)|Ks1], P, E, Q, Ks) :-
	stp_parse_1( Ks1, 1200, E, Q1, Ks2),						% look for "}"
	Q1 = q(_,T1,_,_),
	stp_expect( '}', Ks2, Ks3),
	!,
	Ks2 = [t(_,_,_,Fs2)|_],
	T2 = '{}'(T1),
	Q2 = q(c,T2,[Q1],[Fs1,Fs2]),
	stp_exprtl0( Ks3, T2, Q2, P, E, Q, Ks).

/* this breaks T-Q congruence - don't wanna parse these anyway...
stp_parse_2( var(_), t(var(_),_,Variable,_), [t('(',_,_,_)|S1], Precedence, E, Answer, S) :-
	!,
	stp_parse_1( S1, 999, E, Arg1, S2),		% look for "," or ")"
	stp_stp_read_args( S2, E, RestArgs, S3),
	!,
	Term =.. [call,Variable,Arg1|RestArgs],
	stp_exprtl0( S3, Term, Precedence, Answer, E, S).
 */

stp_parse_2( var(N), [t(var(N),_,V,FsV)|Ks0], P, E, Q, Ks) :-
	T1 = V,
	Q1 = q(v(N),T1,[],[FsV]),
	stp_exprtl0( Ks0, T1, Q1, P, E, Q, Ks).

stp_parse_2( atom, [t(atom,_,A,FsA)|Ks0], P, E, Q, Ks) :-
	stp_read_atom( A, FsA, Ks0, P, E, Q, Ks).

%------------------------------------------------------------------------------

% stp_peepop( +TokensIn, +Env, -TokensOut) :-
%   the first clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority

stp_peepop(
	[t(atom,Errs1,A1,Fs1),t('(',Errs2,A2,Fs2)|Ks1],
	_,
	[t(atom,Errs1,A1,Fs1),t('(',Errs2,A2,Fs2)|Ks1]
) :-
	!.

stp_peepop(
	[t(atom,Errs,A,FsA)|Ks1],
	E,
	[t(infixop(L,P,R),Errs,A,FsA)|Ks1]
) :-
	stp_infixop(A, L, P, R, E).

stp_peepop(
	[t(atom,Errs,A,FsA)|Ks1],
	E,
	[t(postfixop(L,P),Errs,A,FsA)|Ks1]
) :-
	stp_postfixop(A, L, P, E).

stp_peepop( Ks0, _, Ks0).

%------------------------------------------------------------------------------

% stp_possible_right_operand( +Tokens, -Flag) :-
%   When you had :- op(120, fx, !), op(110, xf, !),
%   the input [! a !] was incorrectly rejected.  The reason for this
%   was that the "]" following the "!" should have told us that the
%   second "!" *couldn't* be a prefix operator, but the postfix
%   interpretation was never tried.

stp_possible_right_operand( [],	-1).

stp_possible_right_operand( [H|T], Flag) :-
	H = t(Class,_,_,_),
	stp_possible_right_operand_1( Class, Flag, T).

%------------------------------------------------------------------------------

% stp_possible_right_operand_1( +Class, -Flag, +Tokens) :-
%	When you had :- op(120, fx, !), op(110, xf, !),
%	the input [! a !] was incorrectly rejected.  The reason for this
%	was that the "]" following the "!" should have told us that the
%	second "!" *couldn't* be a prefix operator, but the postfix
%	interpretation was never tried.

stp_possible_right_operand_1( var(_),	 1, _).

stp_possible_right_operand_1( number,	 1, _).

stp_possible_right_operand_1( string,	 1, _).

stp_possible_right_operand_1( '(',	 0, _).

stp_possible_right_operand_1( ')',	-1, _).

stp_possible_right_operand_1( '[',	 0, [t(']',_,_,_)|_]) :-
	!.

stp_possible_right_operand_1( '[',	 1, _).

stp_possible_right_operand_1( ']',	-1, _).

stp_possible_right_operand_1( '{',	 0, [t('}',_,_,_)|_]) :-
	!.

stp_possible_right_operand_1( '{',	 1, _).

stp_possible_right_operand_1( '}',	-1, _).

stp_possible_right_operand_1( ',',	-1, _).

stp_possible_right_operand_1( '|',	-1, _).

stp_possible_right_operand_1( atom,	 0, _).

%------------------------------------------------------------------------------

stp_postfixop( F, P, O, E) :-
  %	(	current_op( O, xf, F)
  %	->	P is O-1
  %	;	current_op( O, yf, F)
  %	->	P is O
  %	).
	memberchk( postfixop(F,P,O), E).

%------------------------------------------------------------------------------

% stp_prefix_is_atom( +TokenList, +Precedence) :-
%	is true when the right context TokenList of a prefix operator
%	of result precedence Precedence forces it to be treated as an
%	atom, e.g. (- = X), p(-), [+], and so on.

stp_prefix_is_atom( [Token|_], Precedence) :-
	stp_prefix_is_atom_1( Token, Precedence).

stp_prefix_is_atom( [],  _).

%------------------------------------------------------------------------------

% stp_prefix_is_atom_1( +Token, +Precedence) :-
%	is true when the right context Token of a prefix operator
%	of result precedence Precedence forces it to be treated as an
%	atom, e.g. (- = X), p(-), [+], and so on.

stp_prefix_is_atom_1( t(infixop(L,_,_),_,_,_), P) :-
	L >= P.

stp_prefix_is_atom_1( t(postfixop(L,_),_,_,_), P) :-
	L >= P.

stp_prefix_is_atom_1( t(')',_,_,_), _).

stp_prefix_is_atom_1( t(']',_,_,_), _).

stp_prefix_is_atom_1( t('}',_,_,_), _).

stp_prefix_is_atom_1( t('|',_,_,_), P) :-
	1100 >= P.

stp_prefix_is_atom_1( t(',',_,_,_), P) :-
	1000 >= P.

%------------------------------------------------------------------------------

% stp_prefixop( +Atom, -PrecedenceO, -PrecedenceQ, +Env) :-
%	The original public-domain code was written to go with a similarly
%	public-domain version of op/3 and current_op/3 where the following
%	three tables were the primary reality.  Whether they are or aren't,
%	only current_op/3 is (currently) directly available to customers.

stp_prefixop( F, O, Q, E) :-
  %	(   current_op( O, fx, F)
  %	->	Q is O-1
  %	;   current_op( O, fy, F)
  %	->	Q is O
  %	).
	memberchk( prefixop(F,O,Q), E).

%------------------------------------------------------------------------------

% stp_put_stp_syntax_error( +Message, +TokensRemaining) :-
%	record Message and length of TokensRemaining,
%	to be picked up at top level after failure

stp_put_stp_syntax_error( Message, TokensRemaining) :-
	(	recorded( syntax_error, _, Ref)		% erase any provisional error
	->	erase( Ref)
	;	true
	),
	length( TokensRemaining, Length),
	recorda( syntax_error, Message-Length, _),
	!,
	fail.

%------------------------------------------------------------------------------

% stp_read_args( +TokensIn, +Env, -Terms, -Qs, -Fragmentss, -TokensOut) :-
%	parses {',' expr(999)} ')' and returns a list of terms (Ts),
%	a list (Qs) of corresponding source-text trees,
%	and a list of lists of text fragments 

stp_read_args( [t((,),_,_,Fs)|Ks1], E, [T|Ts], [Q|Qs], [Fs|Fss], Ks) :-
	!,
	stp_parse_1( Ks1, 999, E, Q, Ks2),
	!,
	Q = q(_,T,_,_),
	stp_read_args( Ks2, E, Ts, Qs, Fss, Ks).

stp_read_args( [t(')',_,_,Fs)|Ks], _, [], [], [Fs], Ks) :-
	!.

stp_read_args( Ks, _, _, _, _, _) :-
	stp_put_stp_syntax_error( [', or )',expected,in,arguments], Ks).

%------------------------------------------------------------------------------

% stp_read_atom( +Atom, +Fragments, +TokensIn, +Precedence, +Env, -Q, -TokensOut) :-

stp_read_atom( '-', FsA, [t(number,_Errs,N,FsN)|Ks1], P, E, Q, Ks) :-
	!,
	Neg is -N,
	stp_number_to_sourcetext_node_type( Neg, Tsn),
	append( FsA, FsN, Fs),
	T1 = Neg,
	Q1 = q(Tsn,N,[],[Fs]),
	stp_exprtl0( Ks1, T1, Q1, P, E, Q, Ks).

stp_read_atom( Functor, FsF, [t('(',_,_,FsP)|Ks1], P, E, Q, Ks) :-
	!,
	stp_parse_1( Ks1, 999, E, Qa, Ks2),		% look for "," or ")"
	Qa = q(_,Arg,_,_),
	stp_read_args( Ks2, E, Args, Qas, Fss, Ks3),
	!,
	append( FsF, FsP, Fs1),
	T1 =.. [Functor,Arg|Args],
	Q1 = q(c,T1,[Qa|Qas],[Fs1|Fss]),
	stp_exprtl0( Ks3, T1, Q1, P, E, Q, Ks).

stp_read_atom( Op, FsOp, Ks0, P, E, Q, Ks) :-
	stp_prefixop( Op, Oprec, Aprec, E),
	!,
	/*	At this point we have consumed Op, which is an atom which might
	be a prefix operator of precedence Prec, or it might just be an
	atom.  We consider two things:  what does the next token tell
	us, and what does the local precedence tell us?  The next token
	tells us that
	it may not be an operator	}, ], ), |, ,, end of list
	it must be an operator		var, number, string, {, [, ' ('
	it might be an operator		atom, '('
	*/
	stp_possible_right_operand( Ks0, Flag),
	(	Flag < 0
	->	T1 = Op,										% can't be a prefix op
		Q1 = q(c,T1,[],[FsOp]),				% in q/4, atom is 0-arity constructor
		stp_exprtl0( Ks0, T1, Q1, P, E, Q, Ks)
	;	Oprec > P
	->	stp_put_stp_syntax_error(
			[prefix,operator,Op,in,context,with,precedence,P],
			Ks0
		)
	;	Flag > 0
	->	stp_parse_1( Ks0, Aprec, E, Qa, Ks1),				% must be a prefix op
		!,
		Qa = q(_,Arg,_,_),
		T1 =.. [Op,Arg],
		Q1 = q(c,T1,[Qa],[FsOp,[]]),
		stp_exprtl( Ks1, Oprec, T1, Q1, P, E, Q, Ks)
	;/* Flag = 0; it MIGHT be an atom */
		stp_peepop( Ks0, E, Ks1),
		stp_prefix_is_atom( Ks1, Oprec),					% can't cut but would like to
		T1 = Op,
		Q1 = q(c,T1,[],[FsOp]),
		stp_exprtl( Ks1, Oprec, T1, Q1, P, E, Q, Ks)
	;/* Flag = 0; it CAN'T be an atom (we tried) */
		stp_parse_1( Ks0, Aprec, E, Qa, Ks1),
		!,
		Qa = q(_,Arg,_,_),
		T1 =.. [Op,Arg],
		Q1 = q(c,T1,[Qa],[FsOp,[]]),
		stp_exprtl( Ks1, Oprec, T1, Q1, P, E, Q, Ks)
	).

stp_read_atom( Atom, FsA, Ks0, P, E, Q, Ks) :-
	T1 = Atom,
	Q1 = q(c,T1,[],[FsA]),
	stp_exprtl0( Ks0, T1, Q1, P, E, Q, Ks).

%------------------------------------------------------------------------------

% stp_read_list( +TokensIn, +Env, -Term, -Q, -FragmentsPre, -FragmentsPost, -TokensOut) :-
%	parses {',' expr(999)} ['|' expr(999)] ']' and returns a list

stp_read_list( [], _, _, _, _, _, _) :-
	stp_put_stp_syntax_error( [', | or ]',expected,in,list], []).

stp_read_list( [K|Ks1], E, T, Q, FsPre, FsPost, Ks) :-
	K = t(Class,_,_,_),
	stp_read_list_1( Class, K, Ks1, E, T, Q, FsPre, FsPost, Ks).

%------------------------------------------------------------------------------

% stp_read_list_1( +Class, +Token, +TokensIn, +Env, -Rest, -Q, -FsPre, -FsPost, -TokensOut) :-

stp_read_list_1( ',', t(',',_,_,Fs1), Ks1, E, T, Q, Fs1, Fs3, Ks) :-
	!,
	stp_parse_1( Ks1, 999, E, Q1, Ks2),
	!,
	Q1 = q(_,T1,_,_),
	stp_read_list( Ks2, E, T2, Q2, Fs2, Fs3, Ks),
	T = [T1|T2],
	Q = q(c,T,[Q1,Q2],[[],Fs2,[]]).

stp_read_list_1( '|', t('|',_,_,Fs1), Ks1, E, T, Q, Fs1, Fs2, Ks) :-
	!,
	stp_parse_1( Ks1, 999, E, Q, Ks2),
	!,
	Q = q(_,T,_,_),
	stp_expect( ']', Ks2, Ks),
	Ks2 = [t(']',_,_,Fs2)|_].

stp_read_list_1( ']', t(']',_,_,Fs), Ks1, _, T, Q, [], Fs, Ks1) :-
	!,
	T = '[]',
	Q = q(c,T,[],[[]]).

stp_read_list_1( _, K, Ks1, _, _, _, _, _, _) :-
	stp_put_stp_syntax_error( [', | or ]',expected,in,list], [K|Ks1]).

%------------------------------------------------------------------------------


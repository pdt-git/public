ct(replaceUB(UpperBound), (
        forLoopT(V0, _, Encl, [_], V4, [_], V6),
    	operationT(V4, V0, Encl, [_, V9], '<', 0),
        literalT(V9, V4, Encl, type(basic, int, 0), _),
        forLoopT(V6, V0, Encl, [_], V12, [_], _),
        operationT(V12, V6, Encl, [_, V17], '<', 0),
		literalT(V17, V12, Encl, type(basic, int, 0), _),
		methodDefT(Encl, Class, 'm', [], type(basic, void, 0), [], Block),
		blockT(Block, Encl, Encl, _),
		new_ids([Local, Init])
	), (
		%add(localDefT(Local, Block, Encl, type(basic, int, 0), 'ub', Init)),
		add(paramDefT(Local, Encl, type(basic, int, 0), 'ub')),
		
		add(literalT(Init, Local, Encl, type(basic, int, 0), UpperBound)),
		replace(methodDefT(Encl, Class, 'm', [Local], type(basic, void, 0), [], Block)),
%		replace(blockT(Block, Encl, Encl, [Local|Elements])),
		replace(identT(V9, V4, Encl, 'ub', Local)),
		replace(identT(V17, V12, Encl, 'ub', Local))
	)).
		









/*
		replace(blockT(Block, Encl, Encl, Elements),
		        blockT(Block, Encl, Encl, [Local|Elements])),
		replace(literalT(V9, V4, Encl, type(basic, int, 0), _),
		        identT(V9, V4, Encl, 'ub', Local)),
		replace(literalT(V17, V12, Encl, type(basic, int, 0), _), 
		        identT(V17, V12, Encl, 'ub', Local))
*/


/*
		replace(blockT(Block, Encl, Encl, [Local|Elements])),
		replace(identT(V9, V4, Encl, 'ub', Local)),
		replace(identT(V17, V12, Encl, 'ub', Local))
*/
ct(replaceUB(UpperBound), (
		forLoopT(B0, B1, Encl, [B3], B4, [B5], B6),
		localDefT(B3,B0, Encl, type(basic, int, 0), _, LB),
		operationT(B4, B0, Encl, [B8, UB], '<', 0),
		forLoopT(B6, B0, Encl, [B11], B12, [B13], B14),
		operationT(B12, B6, Encl, [B16, B17], '<', 0),
		literalT(B17, B12, Encl, type(basic, int, 0), _),
		methodDefT(Encl, Class, 'm', [P1,P2,P3], type(basic, void, 0), [], Block),
		blockT(Block, Encl, Encl, Elements),
		new_ids([V0,V7,V8,V9,V12,V15,V17,V20,V18,V19,V22,V23,LVLB,V26,V27,V28,LVUB,V30,V31,V34,V36,V37,V38,V39,V40,
			V35,V32,V33,V41,V42,V43,V44,V45,V46,V47,V48,V50,V51,V52,RUN,V55])
	), (
add(localDefT(V0, Block, Encl, type(basic, int, 0), 'aub', UB)),
set_parent(UB, V0),

add(localDefT(V7, Block, Encl, type(basic, int, 0), 'slice', V8)),
add(operationT(V8, V7, Encl, [V9, V12], '/', 0)),
	add(getFieldT(V9, V8, Encl, 'null', 'MAXTHREADS', V11)),
	add(identT(V12, V8, Encl, 'aub', V0)),
	
add(localDefT(V15, V16, Encl, type(basic, int, 0), 't', V17)),
add(literalT(V17, V15, Encl, type(basic, int, 0), '0')),
add(operationT(V20, V16, Encl, [V18, V19], '<', 0)),
add(identT(V18, V20, Encl, 't', V15)),
add(getFieldT(V19, V20, Encl, 'null', 'MAXTHREADS', V21)),
add(operationT(V22, V16, Encl, [V23], '++', 1)),
add(identT(V23, V22, Encl, 't', V15)),
add(localDefT(LVLB, V25, Encl, type(basic, int, 0), 'lb', V26)),
add(modifierT(LVLB, 'final')),
add(operationT(V26, LVLB, Encl, [V27, V28], '*', 0)),
add(identT(V27, V26, Encl, 'slice', V7)),
add(identT(V28, V26, Encl, 't', V15)),
add(localDefT(LVUB, V25, Encl, type(basic, int, 0), 'ub', V30)),
add(modifierT(LVUB, 'final')),
add(conditionalT(V30, LVUB, Encl, V31, V32, V33)),
add(operationT(V31, V30, Encl, [V34, V35], '>', 0)),
add(operationT(V34, V31, Encl, [V36, V37], '*', 0)),
add(identT(V36, V34, Encl, 'slice', V7)),
add(precedenceT(V37, V34, Encl, V38)),
add(operationT(V38, V37, Encl, [V39, V40], '+', 0)),
add(identT(V39, V38, Encl, 't', V15)),
add(literalT(V40, V38, Encl, type(basic, int, 0), '1')),
add(identT(V35, V31, Encl, 'aub', V0)),
add(identT(V32, V30, Encl, 'aub', V0)),
add(operationT(V33, V30, Encl, [V41, V42], '*', 0)),
add(identT(V41, V33, Encl, 'slice', V7)),
add(precedenceT(V42, V33, Encl, V43)),
add(operationT(V43, V42, Encl, [V44, V45], '+', 0)),
add(identT(V44, V43, Encl, 't', V15)),
add(literalT(V45, V43, Encl, type(basic, int, 0), '1')),
add(execT(V46, V25, Encl, V47)),
add(applyT(V47, V46, Encl, V48, 'start', [], V49)),
add(precedenceT(V48, V47, Encl, V50)),
add(newClassT(V50, V48, Encl, 'null', [], V51, V52, V, 'null')),
add(identT(V51, V50, Encl, 'Thread', V53)),
add(classDefT(V52, V50, 'ANONYMOUS$V52', [RUN])),
add(methodDefT(RUN, V52, 'run', [], type(basic, void, 0), [], V55)),
add(modifierT(RUN, 'public')),
add(modifierT(P1, 'final')),
add(modifierT(P2, 'final')),
add(modifierT(P3, 'final')),
add(blockT(V55, RUN, RUN, [V56])),
	
		replace(operationT(B4, B0, Encl, [B8, UB], '<', 0)),

		add(identT(RefLVUB, V4, Encl, 'ub', LVUB)),
		replace(identT(LB, V4, Encl, 'lb', LVLB))
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
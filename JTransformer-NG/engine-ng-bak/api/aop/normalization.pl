
/**

* Replace assign operations on implicit field accesses

* with simple assignments and operations:

*

* f += 3

* ~>

* f = f + 3

*/


ct(commonassignopnorm,

(

	/* assignOp ist getFieldT = Rhs */

	assignopT(AssignOp, Parent, Encl, Lhs, Operator, Rhs),


	/* Lhs: Receiver.getField */

	getFieldT(Lhs, AssignOp, Encl, Receiver, Fieldname, Field),
	
	getType(Receiver, Receivertype),
	
	not(Receiver == null),

	/* block ist der assignOp umschliessende Block */

	methodDefT(Encl,_,_,_,_,_,Block),
	
	blockT(Block,Encl,Encl,BlockMember),
	
	atom_concat(Fieldname, Lhs, TempVariableName),
	
	new_ids([LhsCopy, Operation, Tempdef, TempIdent, TempIdent2,Tempassign])

),(
	/* add new local temp variable at the beginning of the block of the enclosing method */
	
	add(localDefT(Tempdef, Block, Encl, Receivertype, TempVariableName, null)),

	replace(blockT(Block,Encl,Encl,BlockMember),
	        blockT(Block,Encl,Encl,[Tempdef|BlockMember])),

	/* LHS of Assignment: Teiltransformation 1: assignOp -> assign */
	
	replace(assignopT(AssignOp, Parent, Encl, Lhs, Operator, Rhs),
  	        assignT(AssignOp, Parent, Encl, Lhs, Operation)),
	
	add(operationT(Operation, AssignOp, Encl, [LhsCopy, Rhs], Operator, 0)),
	
		add(getFieldT(LhsCopy, Operation, Encl, TempIdent2, Fieldname, Field)),
		
		add(identT(TempIdent2, LhsCopy, Encl, TempVariableName, Tempdef)),
		
		set_parent(Rhs, Operation),
	
	
	/* RHS of Assignment: Teiltransformation 2: Lhs, LhsCopy als temporaere Variable auslagern */

	replace(getFieldT(Lhs, AssignOp, Encl, Receiver, Fieldname, Field),
	  getFieldT(Lhs, AssignOp, Encl, Tempassign, Fieldname, Field)),	

%	 add(precedenceT(Precedence,Lhs, Encl,Tempassign)),

	  add(assignT(Tempassign, Lhs, Encl, TempIdent, Receiver)),

 	  set_parent(Receiver,Tempassign),
		
	  add(identT(TempIdent, Tempassign, Encl, TempVariableName, Tempdef))
	) 

).

/*

ifT(V2, V3, VV4, V0, V1, V'null'),
operationT(V0, V2, VV4, [V5, V6], '==', 0),
precedenceT(V5, V0, VV4, V7),
assignopT(V7, V5, VV4, V8, '+', V9),
getFieldT(V8, V7, VV4, V10, 'f', V11),
applyT(V10, V8, VV4, V'null', 'getTest', [], V12),
literalT(V9, V7, VV4, type(basic, int, 0), '4'),
literalT(V6, V0, VV4, type(basic, int, 0), '3'),
blockT(V1, V2, VV4, []),

-->

localDefT(V0, V1, VV2, type(class, Type0, 0), 'tmp$', V'null'),
ifT(V5, V1, VV2, V3, V4, V'null'),
operationT(V3, V5, VV2, [V6, V7], '==', 0),
precedenceT(V6, V3, VV2, V8),
assignT(V8, V6, VV2, V9, V10),
getFieldT(V9, V8, VV2, V11, 'f', V12),
precedenceT(V11, V9, VV2, V13),
assignT(V13, V11, VV2, V14, V15),
identT(V14, V13, VV2, 'tmp$', V0),
applyT(V15, V13, VV2, V'null', 'getTest', [], V16),
operationT(V10, V8, VV2, [V17, V18], '+', 0),
getFieldT(V17, V10, VV2, V19, 'f', V20),
identT(V19, V17, VV2, 'tmp$', V0),
literalT(V18, V10, VV2, type(basic, int, 0), '4'),
literalT(V7, V3, VV2, type(basic, int, 0), '3'),
blockT(V4, V5, VV2, []),


*/
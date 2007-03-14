/**
 * inlined_annotation(AnnotationID, annotation(AnnotationTypeFQN, Args))
 *<p>
 * e.g.
 * annotation('org.cs3.AnnotationX', [(Name=Value),...]).
 * <p>
 * Allowed Values: 
 *  </menu>
 *  <li>annotation(AnnotationID | [AnnotationID1,...])</li>
 *  <li>Atom| [Atom1,...] : literal(ValueType) </li>
 *  <li>FQN|[FQN1,...] : type </li>
 *  <li>EnumTypeName.CONSTX |[EnumTypeName.CONSTX1,...] : enum(EnumFQN)</li>
 * </menu>
 */
inlined_annotation(AnnotationID, annotation(AnnotationTypeFQN, InlinedMemberValues)):-
    var(AnnotationTypeFQN),
    annotationT(AnnotationID, _AnnotatedID, _, Type, MemberValues),
    fullQualifiedName(Type,AnnotationTypeFQN),
    match_members(InlinedMemberValues, MemberValues).

inlined_annotation(AnnotationID, annotation(AnnotationTypeFQN, InlinedMemberValues)):-
    nonvar(AnnotationTypeFQN),
    fullQualifiedName(Type,AnnotationTypeFQN),
    annotationT(AnnotationID, _AnnotatedID, _, Type, MemberValues),
    match_members(InlinedMemberValues, MemberValues).

/**
 * match_members(InlinedMembers, Members):-
 */
match_members([], []).
%match_members([value=Value|InlinedMembers], [Member|Members]):-
%    memberValueT(Member,AnnotationType,null,ValueExpr),
%    class(AnnotationType,_,_,[AnnotationMember]),
%    annotationMemberT(AnnotationMember

match_members([Name=FlatValue|InlinedMembers], [Member|Members]):-
    memberValueT(Member,Annotation,AnnotationMember,Expr),
    member_name(Name,Annotation,AnnotationMember),
    flattenExpr(Expr, FlatValue),
    match_members(InlinedMembers, Members).
    
member_name(Name,Annotation,null):-
    !,
    annotationT(Annotation, _,_, AnnotationType, _ ),
    class(AnnotationType,_,_,[AnnotationMember]),
    annotationMemberT(AnnotationMember,_,_,Name,_).

member_name(Name,_Annotation,AnnotationMember):-
    annotationMemberT(AnnotationMember,_,_,Name,_).
    
    
/**
 * flattenExpr(Expr, Flattened)
 *
 * TODO: conditionals
 */    
flattenExpr(Expr, Flattened:literal(LiteralType)):-
    literalT(Expr,_,_,Type,Flattened), 
    map_type_term(Type,LiteralType).

flattenExpr(Expr, InlinedAnnotation:annotation):-
    annotationT(Expr,_,_,_,_),
    inlined_annotation(Expr,InlinedAnnotation).

flattenExpr(Expr, FlattenedElements:array):-
    newArrayT(Expr,_,_,_,Elements,_),
    maplist(flattenExpr,Elements,FlattenedElements).

flattenExpr(Expr, FQN:type):-
    selectT(Expr,_,_,class,_Ident,TypeTerm),
    map_type_term(TypeTerm,FQN).
  
flattenExpr(Expr, ConstantName:enum(EnumTypeFQN)):-
    getFieldT(Expr,EnumType,_,_,ConstantName,_),
    fullQualifiedName(EnumType,EnumTypeFQN).
  
setUp(inlined_annotation):-
    %%% referenced types %%%
	packageT(PckgType0, 'java.lang'),
	classDefT(Type0, PckgType0, 'String',_),
	classDefT(Annotation, AnnPckg, 'Annotation',_),
	packageT(AnnPckg, 'java.lang.annotation'),
	
%	packageT(PckgAspects, 'aspects'),
%    classDefT(Contributor, PckgAspects, 'DocumentContributor',_),
%    classDefT(ContainerPath, PckgAspects, 'DocumentContainerPath',_),
	
	V5='AnnotationID',
	new_ids([AnnotationClass,TestPackage,
	         VV01,VV21,VV11,
		     VV0,VV1,
		     V1,V2,V3,V4,   V7,V8,V9,V10,
		     V11,V12,V13,V14,V15,V16,V17,V18,V19,
		     V20,V21,V22,V23,V24
		     ]),

	%%% selection %%%
	add(classDefT(AnnotationClass, TestPackage, 'DocumentContributor', [VV11])),
    add(packageT(TestPackage, 'sometestpackage')),
	add(modifierT(VV21, 'public')),
	add(extendsT(VV21, Annotation)),
	add(annotationTypeT(VV21)),
	add(annotationMemberT(VV11, VV21, type(class, Type0, 0), 'value', 'null')),

	add(methodDefT(VV0, VV1, 'toString', [], type(class, V2, 0), [], V3)),
	add(modifierT(VV0, 'public')),
	add(annotatedT(VV0, V5)),
	add(annotationT(V5, VV0, VV0, AnnotationClass, [V4])),
	add(memberValueT(V4, V5, null, V10)),
	add(literalT(V10, V4, VV0, type(class, Type0, 0), 'String')),
%	add(memberValueT(V11, V12, 'null', V13)),
%	add(annotatedT(VV0, V12)),
%	add(annotationT(V12, VV0, VV0, V14, [V11])),
%	add(newArrayT(V13, V11, VV0, [], [V17], type(class, V18, 1))),
%	add(omitArrayDeclarationT(V13)),
%	add(memberValueT(V19, V17, 'null', V20)),
%	add(annotatedT(V13, V17)),
%	add(annotationT(V17, V13, VV0, V21, [V19])),
%	add(literalT(V20, V19, VV0, type(class, Type0, 0), '')),
	add(blockT(V3, VV0, VV0, [V24])).
	
test(inlined_annotation):-
    inlined_annotation('AnnotationID', annotation(AnnotationTypeFQN, InlinedMemberValues)),
    AnnotationTypeFQN = 'sometestpackage.DocumentContributor',		
    InlinedMemberValues = [value='String':literal('java.lang.String')].
    
tearDown(inlined_annotation):-
    rollback.
    

package org.cs3.jtransformer.internal.astvisitor;

public interface Names {
	/**
	 * Modifiers
	 */
	  public String STRICTFP = "strictfp";
	  public String VOLATILE = "volatile";
	  public String TRANSIENT = "transient";
	  public String SYNCHRONIZED = "synchronized";
	  public String NATIVE = "native";
	  public String PROTECTED = "protected";
	  public String FINAL = "final";
	  public String ABSTRACT = "abstract";
	  public String STATIC = "static";
	  public String PRIVATE = "private";
	  public String PUBLIC = "public";
	
	/**
	 * Java 5
	 */
	  public String ANNOTATION_TYPE_T = "annotationTypeT";
	  public String ANNOTATION_T = "annotationT";
	  public String ENUM_T = "enumT";
	  public String ENUM_CONSTANT_T = "enumConstantT";
	  public String ANNOTATION_MEMBER_T = "annotationMemberT";
 	  public String MEMBER_VALUE_T = "memberValueT";
	  public String MARKER_ANNOTATION_T = "markerAnnotationT";
	
	/**
	 * Java 1.4
	 */
	  public String PACKAGE_T = "packageT";
	  public String MODIFIER_T = "modifierT";
	  public String WHILE_LOOP_T = "whileLoopT";
	  public String LOCAL_DEF_T = "localDefT";
	  public String PARAM_DEF_T = "paramDefT";
	  public String IMPLEMENTS_T = "implementsT";
	  public String EXTENDS_T = "extendsT";
	  public String INTERFACE_T = "interfaceT";

	  public String TRY_T = "tryT";
	  public String THROW_T = "throwT";
	  public String SYNCHRONIZED_T = "synchronizedT";
	  public String SWITCH_T = "switchT";
	  public String CASE_T = "caseT";
	  public String SELECT_T = "selectT";
	  public String RETURN_T = "returnT";
	  public String PRECEDENCE_T = "precedenceT";
	  public String IDENT_T = "identT";
	  public String LABEL_T = "labelT";
	  public String METHOD_DEF_T = "methodDefT";
	  public String TYPE_TEST_T = "typeTestT";
	  public String OPERATION_T = "operationT";
	  public String IMPORT_T = "importT";
	  public String IF_T = "ifT";
	  public String FOR_LOOP_T = "forLoopT";
	  public String FOR_EACH_T = "foreachT";
	  public String FIELD_DEF_T = "fieldDefT";
	  public String GET_FIELD_T = "getFieldT";
	  public String NOP_T = "nopT";
	  public String DO_LOOP_T = "doLoopT";
	  public String CONTINUE_T = "continueT";
	  public String CONDITIONAL_T = "conditionalT";
	  public String PROJECT_LOCATION_T = "projectLocationT";
	  public String TOPLEVEL_T = "toplevelT";
	  public String NEW_CLASS_T = "newClassT";
	  public String CATCH_T = "catchT";
	  public String TYPE_CAST_T = "typeCastT";
	  public String BREAK_T = "breakT";
	  public String LITERAL_T = "literalT";
	  public String APPLY_T = "applyT";
	  public String EXEC_T = "execT";
	  public String BLOCK_T = "blockT";
	  public String ASSIGNOP_T = "assignopT";
	  public String ASSIGN_T = "assignT";
	  public String ASSERT_T = "assertT";
	  public String NEW_ARRAY_T = "newArrayT";
	  public String INDEXED_T = "indexedT";
	  public String CLASS_DEF_T = "classDefT";
	  public String SOURCE_LOCATION_T = "slT";
	  public String PROJECT_T = "projectT";
	  public char BRACKET_OPEN = '[';
	  public char BRACKET_CLOSE = ']';
	  
	  public String OMIT_ARRAY_DECLARATION_T = "omitArrayDeclarationT";



	  public String SOURCE_LOCATION_ARGUMENT = "sl_argT";
	  public String ARGUMENT_IDENTIFIER = "identifier";
	  public String ARGUMENT_MODIFIER = "modifier";
	  public String ARGUMENT_OPERATOR = "operator";
	  public String ARGUMENT_INSTANCEOF = "instanceof";
	  public String ARGUMENT_TYPE = "type";
	
	  public String SUPER = "super";
	  public String THIS = "this";
	  public String CONSTRUCTOR_NAME = "<init>";
	  
	  public String PRED_ACTIVATE_REVERSE_INDEXES = "activate_reverse_indexes";
}

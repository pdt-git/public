package org.cs3.jlmp.internal.astvisitor;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.cs3.pl.common.Debug;
import org.eclipse.jdt.core.IBuffer;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IPackageBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.SynchronizedStatement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * Translates a java source file into Prolog facts. This class
 * is the heart of our System, where the mapping from AST nodes
 * to Prolog facts gets done. Due to differences between the
 * Prolog and the Java AST, sometimes we need to maintain
 * some tempoary information, and / or handle some facts in
 * nodes they do not properly belong. This however is transparent
 * to the user, and should not cause problems.
 * 
 * @author schulzs1
 */

public class FactGenerator extends ASTVisitor {
	
	private Hashtable labels = new Hashtable();
	private Hashtable packages = new Hashtable(); 
	private Hashtable syntheticConstructorIds = new Hashtable();
	private FQNTranslator fqnManager; 
	
	protected IPrologWriter writer;
	protected IIDResolver idResolver;	
	
	/**
	 * constructs a new FactGenerator by using the options passed in the toolbox.
	 * @param tb
	 * @param plw
	 */
	
	public FactGenerator(ICompilationUnit icu, String file, FactGenerationToolBox tb, IPrologWriter plw){
		this(icu, file, tb.getIDResolver(), tb.getTypeResolver(), plw, tb.getFQNTranslator());
	}

	 /**
	  * Constructs a new FactGenerator instance for a given ICompilationUnit.
	  * <p>
	  * @param icu the ICompilationUnit from which the AST root was/is created.
	  * @param name the filename given to the topLevelT fact.
	  * @param resolver the IIDResolver  to be used by this FactGenerator.
	  * @param typeResolver the ITypeResolver to be used by this FactGenerator.
	  * @param writer the IPrologWriter to be used by this FactGenerator.
	  */	
	public FactGenerator(ICompilationUnit icu, String name,	IIDResolver resolver,
						 ITypeResolver typeResolver, IPrologWriter writer, FQNTranslator fqnManager) {
			super();
			fileName = name;
			this.writer = writer;
			this.idResolver = resolver;
			this.typeResolver = typeResolver;
			this.iCompilationUnit=icu;
			this.fqnManager = fqnManager;
	}
	
	public FactGenerator (ICompilationUnit icu, String name,	IIDResolver resolver,
						  ITypeResolver typeResolver, IPrologWriter writer){
		this(icu, name, resolver, typeResolver, writer, new IdentityFQNTranslator());
	}
	
	

	/**
	 * the file name given to the generated topLevelT.
	 */
	protected String fileName;

	/* the underlying ICompilationUnit instance.
	 * 
	 * this should be set during initialization and should
	 * reference an ICompilationUnit that is in "WorkingCopy-mode".
	 * 
	 * <p> Beware, nontheless: using the old (and deprecated!) constructor
	 * will not initialize this field, so it will be null.
	 */
	
	private ICompilationUnit iCompilationUnit = null;
	
	private ITypeResolver typeResolver;

	public final static String CONSTRUCTOR_NAME = "<init>";

	private String INITIALIZER_NAME = "<clinit>";
	
	/** 
	 * Generates prolog facts of type classDefT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ClassDefT">
	 * classDefT<a>}
	 * @see FactGeneratorInterfaceTest#testTypeDeclaration()
	 */
	
	public boolean visit(AnonymousClassDeclaration node) {
		String name;
		ASTNode no = getEnclosingNode(node);
		
		String[] args =
			new String[] {
				idResolver.getID(node),
				idResolver.getID(node.getParent()),
				"'ANONYMOUS$" + idResolver.getID(node) + "'",
				idResolver.getIDs(expandList(node.bodyDeclarations().iterator()))
			};

		writer.writeFact("classDefT", args);
		writer.writeFact("slT", new String [] {
				idResolver.getID(node),
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});

		return true;
	}
	
	/** 
	 * Generates prolog facts of type indexedT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=IndexedT">
	 * indexedT<a>}
	 * @see FactGeneratorTest3#testArrayAccess()
	 */
	
	public boolean visit(ArrayAccess node) {
		String[] args =
			new String[] {
				idResolver.getID(node.getIndex()),
				idResolver.getID(node.getArray())};

		createBodyFact(node, "indexedT", args);
		return true;
	}
	
	/** 
	 * Generates prolog facts of type newArrayT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=NewArrayT">
	 * newArrayT<a>}
	 * @see FactGeneratorTest3b#testArrayCreation()
	 */
	
	public boolean visit(ArrayCreation node) {
		String [] arg;
		if(node.getInitializer() != null)
			idResolver.setEquivalence(node,node.getInitializer());
		
		List childArrays = new ArrayList();
		List nextList = new ArrayList();
		
		
		
		StringBuffer dimbuffer = new StringBuffer("[");
		List dims = node.dimensions();
		String elems;
		
		// FIXME: use resolveType!
		Type memberType = node.getType().getComponentType();
		
		boolean implicitDimensions = true;
		
		boolean first = true;
		
		for (Iterator i = dims.iterator(); i.hasNext();) {
			implicitDimensions = false;
			if (!first)
				dimbuffer.append(", ");
			else 
				first = false;
			Expression e = (Expression) i.next();
			dimbuffer.append(idResolver.getID(e));
		}
		
		dimbuffer.append("]");
		
		String dms = dimbuffer.toString();
		
		if (!implicitDimensions){
			elems = getEmptyList();
			String [] args = {
					dms, 
					elems,	
					typeResolver.getTypeTerm(node.resolveTypeBinding())
			};
			
			createBodyFact(node,"newArrayT", args); 
		}
		
		return true;
	}
	
	/** 
	 * Only for arrays that have an explicit initializer  
	 */
	public boolean visit(ArrayInitializer node) {
				
		String id = idResolver.getID(node);
		String parent;
		if (node.getParent().getNodeType() == ASTNode.ARRAY_CREATION)
			parent = idResolver.getID(node.getParent().getParent());
		else
			parent = idResolver.getID(node.getParent());
		String enclosing = idResolver.getID(getEnclosingNode(node));
		String dims = getEmptyList();
		String init = idResolver.getIDs(node.expressions());
		String type = typeResolver.getTypeTerm(node.resolveTypeBinding());
			
		String [] arr = {id, parent, enclosing, dims, init, type};
		
		/*
		 * can't use createBodyFact here, since parent is not always the actual parent
		 */
		
		writer.writeFact("newArrayT", arr);
		writer.writeFact("slT", new String [] {
				idResolver.getID(node),
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});
				
		return true;
	}
	
	/** 
	 * Generates prolog facts of type assertT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=AssertT">
	 * assertT<a>}
	 * @see FactGeneratorTest3b#testAssertStatement()
	 */
	public boolean visit(AssertStatement node) {
		String args[] =
			new String[] {
				idResolver.getID(node.getExpression()),
				idResolver.getID(node.getMessage())
			};
		
		createBodyFact(node, "assertT", args);
		return true;
	}
	
	/** 
	 * Generates prolog facts of type assignT and assignopT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=AssignT">
	 * assignT<a>}
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=AssignopT">
	 * assignopT<a>}
	 * @see FactGeneratorTest3#testAssignmentConditionalExpressionAndExpressionStatement()
 	 */
	public boolean visit(Assignment node) {
		String lhs = idResolver.getID(node.getLeftHandSide());
		if (node.getOperator() == Assignment.Operator.ASSIGN) {
			String args[] =
				new String[] {
					lhs, 
					idResolver.getID(node.getRightHandSide())};

			createBodyFact(node, "assignT", args);
		} else {
			Assignment.Operator op = node.getOperator();
			String opcode =
				op.toString().substring(0, op.toString().length() - 1);
			String args[] =
				new String[] {
					lhs,
					"'" + opcode + "'",
					idResolver.getID(node.getRightHandSide())};

			createBodyFact(node, "assignopT", args);
		}
		
		return true;
	}
	
	/**
	 * Generates prolog facts of type blockT. This includes SuperConstructorInvocation (see
	 * source comment!)
	 * 
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=BlockT">
	 * blockT<a>}
	 * @see  FactGeneratorCodeTest#testBlockT() 
	 */
	public boolean visit(Block node) {

		if (node.getParent().getNodeType() == ASTNode.METHOD_DECLARATION && node.statements().size() > 0 && node.statements().get(0) instanceof SuperConstructorInvocation){
			/* we have a SuperConstructorInvocation. This means we need to generate
			 * a new ID for the execT we plan to add, and put this into
			 * the list of Statements, instead of the real id for the Node (which
			 * will be used for the applyT since the parameters back-reference
			 * to it). We tempoarily remove the invocation, generate the list
			 * of the other Statement's IDs, and then prepend the execT's id
			 * to the list. Then we generate properly linked facts for the
			 * node, and write the block itself. Finally, we recurse down, 
			 * ignoring the SuperConstructorInvocation node (its visit() being
			 * empty).
			 */
			
			List statements = node.statements();
			SuperConstructorInvocation sci = (SuperConstructorInvocation) statements.get(0);
			String constructorID = idResolver.getID(sci);
			String execID = idResolver.getID();
			statements.remove(0);
			String childrenlist=idResolver.getIDs(expandList(node.statements().iterator()));
			if (!childrenlist.equals("[]"))
				childrenlist = "[" + execID + ", " + childrenlist.substring(1);
			else
				childrenlist = "[" + execID + "]";
			statements.add(0, sci);
			
			
			String[] execArgs =
				new String[] {
					execID,
					idResolver.getID(node),
					idResolver.getID(getEnclosingNode(node)),
					constructorID
			};
			String[] applyArgs =
				new String[] {
					constructorID,
					execID,
					idResolver.getID(getEnclosingNode(node)),
					idResolver.getID(sci.getExpression()),
					quote("super"),
					idResolver.getIDs(sci.arguments()),
					idResolver.getID(sci.resolveConstructorBinding())
			};
			String args[] =
				new String[] {
					childrenlist
			};
			
			createBodyFact(node, "blockT", args);
			writer.addIndention();
			writer.writeFact("execT", execArgs);
			writer.writeFact("applyT", applyArgs);
			writer.writeFact("slT", new String [] {
					idResolver.getID(sci),
					Integer.toString(sci.getStartPosition()),
					Integer.toString(sci.getLength())
			});
			
		} else if (node.getParent().getNodeType() == ASTNode.METHOD_DECLARATION && node.statements().size() > 0 && node.statements().get(0) instanceof ConstructorInvocation){
			/* StS
			 * 
			 * We have a ConstructorInvocation. So we need to do the same happy
			 * dance we did for the SuperConstructorInvocation. No, i don't like
			 * it either.
			 */
			
			List statements = node.statements();
			ConstructorInvocation sci = (ConstructorInvocation) statements.get(0);
			String constructorID = idResolver.getID(sci);
			String execID = idResolver.getID();
			statements.remove(0);
			String childrenlist=idResolver.getIDs(expandList(node.statements().iterator()));
			if (!childrenlist.equals("[]"))
				childrenlist = "[" + execID + ", " + childrenlist.substring(1);
			else
				childrenlist = "[" + execID + "]";
			statements.add(0, sci);
			
			
			String[] execArgs =
				new String[] {
					execID,
					idResolver.getID(node),
					idResolver.getID(getEnclosingNode(node)),
					constructorID
			};
			String[] applyArgs =
				new String[] {
					constructorID,
					execID,
					idResolver.getID(getEnclosingNode(node)),
					quote("null"),
					quote("this"),
					idResolver.getIDs(sci.arguments()),
					idResolver.getID(sci.resolveConstructorBinding())
			};
			String args[] =
				new String[] {
					childrenlist
			};
			
			createBodyFact(node, "blockT", args);
			writer.addIndention();
			writer.writeFact("execT", execArgs);
			writer.writeFact("applyT", applyArgs);	
			writer.writeFact("slT", new String [] {
					idResolver.getID(sci),
					Integer.toString(sci.getStartPosition()),
					Integer.toString(sci.getLength())
			});
		} else {
			String childrenlist=idResolver.getIDs(expandList(node.statements().iterator()));
		
			String args[] =
				new String[] {
					childrenlist
			};

			createBodyFact(node, "blockT", args);
			writer.addIndention();
		}
		
		
		return true;
	}
	/**
	 * Generates prolog facts of type literalT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LiteralT">
	 * literalT<a>}
	 * @see FactGeneratorCodeTest#testBooleanLiteralT()
	 */
	
	public boolean visit(BooleanLiteral node) {
		String args[] =
			new String[] {
				typeResolver.getTypeTerm(node),
				"'" + node.booleanValue() + "'" };
		createBodyFact(node, "literalT", args);
		return true;
	}
	
	/**
	 * Generates prolog facts of type breakT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=BreakT">
	 * breakT<a>}
	 * @see  FactGeneratorTest3b#testBreakStatementAndLabledStatement()
	 */
	public boolean visit(BreakStatement node) {
		String label;
		String target;
		
		SimpleName n = node.getLabel();
		
		if (n != null){
			label = quote(n); 
			target = idResolver.getID((ASTNode)labels.get(n.getIdentifier()));
		} else {
			label = "'null'";
				target = idResolver.getID(getEnclosingIterator(node));
				
			
		} 
		
		String[] args =
			new String[] {
				label,
				target
				};

		createBodyFact(node, "breakT", args);

		return true;
	}
	/** 
	 * Generates prolog facts of type typeCastT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=TypeCastT">
	 * typeCastT<a>}
	 * @see FactGeneratorTest3#testCastExpression() 
	 */
	public boolean visit(CastExpression node) {
		String[] args =
			new String[] {
				typeResolver.getTypeTerm(node.resolveTypeBinding()),
				idResolver.getID(node.getExpression())};

		createBodyFact(node, "typeCastT", args);

		return true;
	}
	/**
	 * Generates prolog facts of type catchT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=CatchT">
	 * catchT<a>}
	 * @see FactGeneratorTest3#testTryCatch()
   	 */
	public boolean visit(CatchClause node) {
		String[] args =
			new String[] {
				idResolver.getID(node.getException()),
				idResolver.getID(node.getBody())};

		createBodyFact(node,"catchT", args);

		return true;
	}
	/**
	 * get source code for a node.
	 * @param node
	 * @return the source code coresponding to this node or null if none
	 * is available. 
	 */
	IBuffer buffer = null;
	
	public String getTextForNode(ASTNode node){
		return getTextForNode(node, 0,0);
	}
	
	/**
	 * get source code for a node with offset and length diff.
	 * @param node
	 * @return the source code coresponding to this node or null if none
	 * is available. 
	 */
	public String getTextForNode(ASTNode node,int offsetdiff, int lendiff){
		int startPos = node.getStartPosition();
		int length = node.getLength();
		if(length<=0) return null;
		//FIXME: ld: this assumes, expects, needs, hopes, that the
		//iCompilationUnit is already in WorkingCopymode. Let's hope
		//this assumption is not to strong.
		try {
			if(buffer == null)
				buffer = iCompilationUnit.getBuffer();
		} catch (JavaModelException e) {
			// TODO Auto-generated catch block
			Debug.report(e);
		}
		return buffer.getText(startPos+ offsetdiff,length+lendiff-offsetdiff);		
	}
	/**
	 * Generates prolog facts of type literalT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LiteralT">
	 * literalT<a>}
	 * @see FactGeneratorCodeTest#testCharacterLiteral()
	 */
	public boolean visit(CharacterLiteral node) {
		String type = typeResolver.getTypeTerm(node);
		//ld: we don't care about the charValue anymore, since we can
		//get the literal text "literaly" from the source code. 
		//char cvalue = node.charValue();
		String contents = getTextForNode(node).substring(1,node.getLength()-1);
		//ld: carefull: we have to escape escape chars, like a backslash
				
		String value = contents.replaceAll("\\\\","\\\\\\\\");
		value = value.replaceAll("'","\\\\'");

		String[] args = new String[] { type, quote(value) };
		createBodyFact(node, "literalT", args);
		return false;
	}
	/** 
	 * Generates prolog facts of type newClassT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=NewClassT">
	 * newClassT<a>}
	 * @see FactGeneratorTest3#testInstanceCreation()
	 */
	public boolean visit(ClassInstanceCreation node) { 
		String constructor; 
		String arg1 = idResolver.getIDs(node.arguments()); 
		String type = idResolver.getID(node.getName());
		String def = idResolver.getID(node.getAnonymousClassDeclaration());
		
	
		ITypeBinding typeBinding = node.resolveTypeBinding();
		constructor = (String)syntheticConstructorIds.get(typeBinding);
		
		if(constructor == null) // Constructor is NOT a synthetic constructor of local class  
			constructor = idResolver.getID(node.resolveConstructorBinding());
		
		String enclose = idResolver.getID(node.getExpression());
		
		if (node.getAnonymousClassDeclaration() != null){
			/* in an anonymous class, there is always a synthetic
			 * constructor, but it is very polymorphic, and can
			 * not be accurately modeled here, since too many
			 * cases exist (variable argument amounts, inner classes
			 * deriving interfaces etc. So we just put null in, and 
			 * are happy
			 */
			
			constructor = "'null'";
		}
		
		String [] args = new String [] {
				constructor,
				arg1,
				type,
				def,
				enclose
		};
		 
		createBodyFact(node, "newClassT", args);
		
	//	handleSelectsIdents(node.getName(), null);
		return true;
	}
	
	/**
	 * Generates prolog facts of type toplevelT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ToplevelT">
	 * toplevel<a>}
	 * @see FactGeneratorInterfaceTest#testCompilationUnit() 
	 */
	
	public boolean visit(CompilationUnit node) {
		initComments(node);
		String id = idResolver.getID(node);
		String pckg = idResolver.getID(node.getPackage());

		List defList = new ArrayList(node.imports());
		defList.addAll(node.types());
		String defs = idResolver.getIDs(defList);

		String[] args = new String[] { id, pckg, quote(fileName), defs };
		writer.writeFact("toplevelT", args);
		IJavaElement pfr = iCompilationUnit.getParent();
		while(pfr.getElementType()!=IJavaElement.PACKAGE_FRAGMENT_ROOT){
			pfr=pfr.getParent();
		}
		args = new String[] { 
				id, 
				quote(iCompilationUnit.getResource().getProject().getName()),
			    quote(pfr.getResource().getProjectRelativePath().toString())};
		writer.writeFact("projectLocationT", args);
		writer.writeFact("slT", new String [] {
				idResolver.getID(node),
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});
		return true;
	}

	Hashtable annotations;
	private void initComments(CompilationUnit node) {
		annotations = new Hashtable();
		for (Iterator iter = node.getCommentList().iterator(); iter.hasNext();) {
			Comment comment = (Comment) iter.next();
			if(comment.isBlockComment() )
				try {
					annotations.put(
						""+(comment.getStartPosition() + comment.getLength()-1),
						parseAnnotation(getTextForNode(comment,2,-2)));

				} catch(IllegalArgumentException iae){}
		}
	}

	private GenericAnnotation getCorrespondingGenericAnnotation(ASTNode node){
		return (GenericAnnotation)annotations.get(""+(node.getStartPosition()-1));
	}

	private GenericAnnotation parseAnnotation(String text) throws IllegalArgumentException {
	
		if(!text.startsWith("@@"))
			throw new IllegalArgumentException();
		StringTokenizer tokenizer = new StringTokenizer(text,"@(,) \n\r\t");
		String name;
		List args = new ArrayList();
		if(tokenizer.hasMoreTokens())
			name = checkToken(tokenizer.nextToken());
		else
			throw new IllegalArgumentException();
		while(tokenizer.hasMoreElements())
			args.add(checkToken(tokenizer.nextToken()));
		return new GenericAnnotationImpl(name, args);
	}

	private String checkToken(String string) {
		if(Character.isLowerCase(string.charAt(0))||
			string.charAt(0) =='\'' && string.charAt(string.length()-1)=='\'')
			return string;
		try{
			Integer.parseInt(string);
		}catch(NumberFormatException nfe){
			throw new IllegalArgumentException();
		}
		return string;
	}

	/** 
	 * Generates prolog facts of type conditionalT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ConditionalT">
	 * conditionalT<a>}
	 * @see FactGeneratorTest3#testAssignmentConditionalExpressionAndExpressionStatement()
	 */
	public boolean visit(ConditionalExpression node) {
		String condition = idResolver.getID(node.getExpression());
		String thenPart = idResolver.getID(node.getThenExpression());
		String elsePart = idResolver.getID(node.getElseExpression());
		String[] args =
			new String[] { condition, thenPart, elsePart };
		createBodyFact(node, "conditionalT", args);

		return true;
	}
	
	/**
	 * Generates prolog facts of type applyT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ApplyT">
	 * applyT<a>}
   */
	public boolean visit(ConstructorInvocation node) {
		return true;
	}
	/** 
	 * Generates prolog facts of type continueT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ContinueT">
	 * continueT<a>}
	 * @see FactGeneratorTest3#testContinueStatementAndLabledStatement()
	 */
	public boolean visit(ContinueStatement node) {
		SimpleName l = node.getLabel();
		
		String target;
		String label;
		
		
		if (l != null){
			label = quote(l.getIdentifier());
			target = idResolver.getID((ASTNode)labels.get(l.getIdentifier()));
		} else {
			label = "'null'";
			target = idResolver.getID(getEnclosingIterator(node));
		}
				
		String[] args =
			new String[] {
				
				label,
				target };
		createBodyFact(node, "continueT", args);
		return true;
	}
	
	/** 
	 * Generates prolog facts of type doLoopT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=DoLoopT">
	 * doLoopT<a>}
	 * @see FactGeneratorTest3#testDoStatement()
	 */
	public boolean visit(DoStatement node) {
		String condition = idResolver.getID(node.getExpression());
		String body = idResolver.getID(node.getBody());
		String[] args = new String[] {  condition, body };
		createBodyFact(node, "doLoopT", args);
		return true;
	}
	/** 
	 * Generates prolog facts of type nopT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=NopT">
	 * nopT<a>}
	 * @see FactGeneratorTest3#testEmptyStatement()
	 */
	public boolean visit(EmptyStatement node) {
		String[] args =
			new String[] {};
		createBodyFact(node,"nopT",args);
		//writer.writeFact("nopT", args);
		return true;
	}
	/** 
	 * Generates prolog facts of type execT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ExecT">
	 * execT<a>}
	 * @see FactGeneratorTest3#testAssignmentConditionalExpressionAndExpressionStatement()
	 */
	public boolean visit(ExpressionStatement node) {
		String[] args =
			new String[] {
				idResolver.getID(node.getExpression())};

		createBodyFact(node, "execT", args);

		return true;
	}
	/**
	 * Generates prolog facts of type getFieldT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=GetFieldT">
	 * getFieldT<a>}
	 * @see FactGeneratorCodeTest#testFieldAccess()
	 */
	public boolean visit(FieldAccess node) {
		String expr = idResolver.getID(node.getExpression());
		String name = quote(node.getName().getIdentifier());
		String field = idResolver.getID(node.resolveFieldBinding());

		String[] args = new String[] {  expr, name,field };

		createBodyFact(node, "getFieldT", args);
			
		return true;
	}
	/**
	 * Generates prolog facts of type fieldDefT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=FieldDefT">
	 * fieldDefT<a>}
	 * @see FactGeneratorInterfaceTest#testFieldDeclaration() 
	 */
	public boolean visit(FieldDeclaration node) {
		List list = (List) node.fragments();
		int size = list.size();
		for (int i = 0; i < size; i++) {
			VariableDeclarationFragment fragment =
				(VariableDeclarationFragment) list.get(i);
			String id = idResolver.getID(fragment);
			String parentId = idResolver.getID(node.getParent());
			
			String type = typeResolver.getTypeTerm(fragment.resolveBinding().getType());
			String name = quote(fragment.getName().toString());
			String init = idResolver.getID(fragment.getInitializer());
			
			String[] args = new String[] { id, parentId, type, name, init };
			
			writer.writeFact("fieldDefT", args);
			writer.writeFact("slT", new String [] {
					idResolver.getID(node),
					Integer.toString(node.getStartPosition()),
					Integer.toString(node.getLength())
			});
			writeModifiers(fragment, node.getModifiers());
		}
		return true;
	}
	/** 
	 * Generates prolog facts of type forLoopT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ForLoopT">
	 * forLoopT<a>}
	 * @see FactGeneratorTest3#testForStatement()
	 */
	public boolean visit(ForStatement node) {
		
		List inits = node.initializers();
		List allInits = new ArrayList();
		
		for (Iterator i = inits.iterator(); i.hasNext();) {
			ASTNode next = (ASTNode) i.next();
			if (next instanceof VariableDeclarationExpression){
				VariableDeclarationExpression el = (VariableDeclarationExpression) next;
				allInits.addAll(el.fragments());
			} else
				allInits.add(next);
		}
		
		String[] args =
			new String[] {
				idResolver.getIDs(allInits),
				idResolver.getID(node.getExpression()),
				idResolver.getIDs(node.updaters()),
				idResolver.getID(node.getBody())};

		createBodyFact(node,"forLoopT", args);

		return true;
	}
	/** 
	 * Generates prolog facts of type ifT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=IfT">
	 * ifT<a>}
	 * @see FactGeneratorTest3#testIfStatement()
	 */
	public boolean visit(IfStatement node) {
		String[] args =
			new String[] {
				idResolver.getID(node.getExpression()),
				idResolver.getID(node.getThenStatement()),
				idResolver.getID(node.getElseStatement())};
		createBodyFact(node, "ifT", args);
		return true;
	}
	
	/**
	 * Generates prolog facts of type importT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ImportT">
	 * importT<a>}
	 * @see  FactGeneratorInterfaceTest#testImportDeclaration()
	 */
	public boolean _visit(ImportDeclaration node) {
		String id = idResolver.getID(node);
		String topLevel = idResolver.getID(node.getParent());
		String importName = idResolver.getID(node.resolveBinding());
		IBinding binding =  node.resolveBinding();
		if (binding.getKind() == IBinding.PACKAGE) 
			generatePackageFactIfNecessary((IPackageBinding) binding);
		importName = idResolver.getID(binding);
		String[] args = new String[] { id, topLevel, importName };
		writer.writeFact("importT", args);
		writer.writeFact("slT", new String [] {
				idResolver.getID(node),
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});
		return false;
	}

	/**
	 * Generates prolog facts of type importT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ImportT">
	 * importT<a>}
	 * @see  FactGeneratorInterfaceTest#testImportDeclaration()
	 */
	public boolean visit(ImportDeclaration node) {
		String id = idResolver.getID(node);
		String topLevel = idResolver.getID(node.getParent());
		String importName = idResolver.getID(node.resolveBinding());
		IBinding binding =  node.resolveBinding();
		if (binding.getKind() == IBinding.PACKAGE) 
			generatePackageFactIfNecessary((IPackageBinding) binding);
		importName = idResolver.getID(binding);
		String[] args = new String[] { id, topLevel, importName };
		writer.writeFact("importT", args);
		writer.writeFact("slT", new String [] {
				idResolver.getID(node),
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});
		return false;
	}
	
	/**
	 * Generates prolog facts of type operationT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=OperationT">
	 * operationT<a>}
	 * @see  FactGeneratorTest3#testInfixExpression()
	 */
	public boolean visit(InfixExpression node) {
		//ld: changed to respect extended operands. 
		//    please see api docs for InfixExpression$extendedOperands()
		StringBuffer operands=new StringBuffer();
		operands.append("[");
		operands.append(idResolver.getID(node.getLeftOperand()));
		operands.append(", ");
		operands.append(idResolver.getID(node.getRightOperand()));
		for (Iterator it = node.extendedOperands().iterator(); it.hasNext();) {
			Expression extop = (Expression) it.next();
			operands.append(", ");
			operands.append(idResolver.getID(extop));
		}
		operands.append("]");
		String[] args =
			new String[] {
				operands.toString(),
				"'" + node.getOperator().toString() + "'",
				"0" };

		createBodyFact(node, "operationT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type typeTestT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=TypeTestT">
	 * typeTestT<a>}
	 * @see  FactGeneratorTest3#testInstanceOFExpression()
	 */
	public boolean visit(InstanceofExpression node) {
		String[] args =
			new String[] {
				typeResolver.getTypeTerm(node.getRightOperand()),
				idResolver.getID(node.getLeftOperand())};

		createBodyFact(node, "typeTestT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type methodDefT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=MethodDefT">
	 * methodDef<a>}
	 * @see  FactGeneratorMethodBodyTest#testVisitInitializer() 
	 */
	public boolean visit(Initializer node) {

		String id = idResolver.getID(node);
		String parentId = idResolver.getID(node.getParent());
		String name = quote(INITIALIZER_NAME);
		String param = getEmptyList();
		//		Type nodetype = ((TypeDeclaration)node.getParent()).
		String type = "null"; //idResolver.getTypeTerm(nodetype);
		String exceptions = getEmptyList();
		String body = idResolver.getID(node.getBody());

		String[] args =
			new String[] { id, parentId, name, param, type, exceptions, body };
		writer.writeFact("methodDefT", args);
		
		writer.writeFact("slT", new String [] {
				idResolver.getID(node),
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});

		//ld: n. verein. brauchen wir das bei nem static block nicht.
		writeModifiers(node, node.getModifiers());
		return true;
	}
	/**
	 * not implemented
	 */
	public boolean visit(Javadoc node) {
		/* TODO: Implement something for me! IGNORED for Story 1! */
		return false;
	}
	/**
	 * Generates prolog facts of type labelT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LabelT">
	 * labelT<a>}
	 * @see  FactGeneratorTest3b#testBreakStatementAndLabledStatement()
	 * @see  FactGeneratorTest3#testContinueStatementAndLabledStatement()
	 */
	public boolean visit(LabeledStatement node) {
		labels.put(node.getLabel().getIdentifier(), node);

		String body = idResolver.getID(node.getBody());
		String name = quote(node.getLabel().getIdentifier());
		String[] args = new String[] {  body, name };

		createBodyFact(node, "labelT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type methodDefT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=MethodDefT">
	 * methodDef<a>}
	 * @see  FactGeneratorInterfaceTest#testMethodDeclaration()
	 */
	public boolean visit(MethodDeclaration node) {

		String id = idResolver.getID(node);
		String parentId = idResolver.getID(node.getParent());

		String name =
			quote(
				node.isConstructor()
					? CONSTRUCTOR_NAME
					: node.getName().toString());
		
		String param = idResolver.getIDs(node.parameters());
		
		String type = typeResolver.getTypeTerm(node.resolveBinding().getReturnType());
		List exs = node.thrownExceptions();
		List exsBind = new ArrayList();
		for (Iterator iter = exs.iterator(); iter.hasNext();) {
			Name ex = (Name) iter.next();
			exsBind.add(ex.resolveBinding());
		}
		String exceptions = idResolver.getIDs(exsBind);
		
		String body = idResolver.getID(node.getBody());

		String[] args =
			new String[] { id, parentId, name, param, type, exceptions, body };
		writer.writeFact("methodDefT", args);
		writer.writeFact("slT", new String [] {
				idResolver.getID(node),
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});

		writeModifiers(node, node.getModifiers());
		// TODO initializer ;clinit;
		//		writer.writeFact("methodDefT",args);
		return true;
	}
	/**
	 * Generates prolog facts of type applyT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ApplyT">
	 * applyT<a>}
	 * @see FactGeneratorMethodBodyTest#testVisitMethodInvocation()
	 * @see FactGeneratorMethodBodyTest#testVisitMethodInvocation_nullExpression()
	 * @see FactGeneratorMethodBodyTest#testVisitMethodInvocation_withArgs()
	 */
	public boolean visit(MethodInvocation node) {
		
		String[] args =
			new String[] {
				idResolver.getID(node.getExpression()),
				quote(node.getName().getIdentifier()),
				idResolver.getIDs(node.arguments()),
				idResolver.getID(node.resolveMethodBinding())};

		createBodyFact(node, "applyT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type identT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=IdentT">
	 * identT<a>}
	 * @see FactGeneratorCodeTest#testFieldAccess()
	 */

	public boolean visit(NullLiteral node) {
		String[] args =
			new String[] {
				quote("null"),
				quote("null")};
		createBodyFact(node, "identT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type literalT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LiteralT">
	 * literalT<a>}
	 * 
	 * @see FactGeneratorCodeTest#testNumberLiteral()
	 */
	public boolean visit(NumberLiteral node) {
		String[] args =
			new String[] {
				typeResolver.getTypeTerm(node),
				"'" + node.getToken() + "'" };
		createBodyFact(node, "literalT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type packageT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=PackageT">
	 * packageT<a>}
	 * @see FactGeneratorInterfaceTest#testPackageDeclaration()
	 */
	public boolean visit(PackageDeclaration node) {
		generatePackageFactIfNecessary(node.resolveBinding());
		return false;
	}
	/**
	 * Generates prolog facts of type precedenceT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=PrecedenceT">
	 * precedenceT<a>}
	 * <p>
	 * New predicate: precedenceT shows parentheses in the source file, and
	 * translates them to Prolog facts 
	 * @see  FactGeneratorTest3b#testParenthesis()
	*/
	public boolean visit(ParenthesizedExpression node) {
		String[] args =
			{
				idResolver.getID(node.getExpression())};

		createBodyFact(node, "precedenceT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type operationT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=OperationT">
	 * operationT<a>}
	 * @see  FactGeneratorTest3#testPrePostExpression()()
	 */
	public boolean visit(PostfixExpression node) {
		String[] args =
			new String[] {
				"[" + idResolver.getID(node.getOperand()) + "]",
				"'" + node.getOperator().toString() + "'",
				"1" };

		createBodyFact(node, "operationT", args);

		return true;
	}
	/**
	 * Generates prolog facts of type operationT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=OperationT">
	 * operationT<a>}
	 * @see  FactGeneratorTest3#testPrePostExpression()()
	 */
	public boolean visit(PrefixExpression node) {
		String[] args =
			new String[] {
				"[" + idResolver.getID(node.getOperand()) + "]",
				"'" + node.getOperator().toString() + "'",
				"-1" };

		createBodyFact(node, "operationT", args);

		return true;
	}
	/**
	 * not implemented, because type terms are generated by the parent nodes.
	 */
	public boolean visit(PrimitiveType node) {

		return true;
	}
	
	/**
	 * Generates prolog facts of type selectT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=SelectT">
	 * selectT<a>}
	 * 
	 * @see FactGeneratorCodeTest#testStringCreation()
 	*/
	public boolean visit(QualifiedName node) {
		String name = node.getName().getIdentifier().toString();
		
		if (node.getParent().getNodeType() == ASTNode.TYPE_DECLARATION)
			return false;
		//ld: if the qn belongs to a throw decl. of a method decl. ...
		if (node.getParent().getNodeType() == ASTNode.METHOD_DECLARATION)
			return false;
		

		if (node.resolveBinding().getKind() == IBinding.TYPE){
			handleSelectsIdents(node, null);
			return false;
		}

		
		if(node.resolveBinding().getKind() == IBinding.PACKAGE)
			generatePackageFactIfNecessary(node.resolveBinding());
		
		generateGetFieldIfNodeIsFieldAccess(node, name);
		
		
/*			String[] args =
				new String[] {
					idResolver.getID(node),
					idResolver.getID(node.getParent()),
					idResolver.getID(getEnclosingNode(node)),
					quote(name),
					idResolver.getID(node.getQualifier()),
					idResolver.getID(node.resolveBinding())};
			writer.writeFact("selectT", args);

		}			*/
		
		
		return false;
	}
	/**
	 * Generates prolog facts of type returnT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ReturnT">
	 * returnT<a>}
	 * @see  FactGeneratorMethodBodyTest#testVisitReturnStatement()
	 * @see  FactGeneratorMethodBodyTest#testVisitReturnStatement_nullExpression()
	 */
	public boolean visit(ReturnStatement node) {
		String[] args =
			new String[] {
				idResolver.getID(node.getExpression())};

		createBodyFact(node, "returnT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type identT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=IdentT">
	 * identT<a>}
	 * @see  FactGeneratorCodeTest#testSimpleName()
   */
	public boolean visit(SimpleName node) {
		
		if (node.getParent().getNodeType() == ASTNode.TYPE_DECLARATION)
			return false;
		
		if (node.getParent().getNodeType() == ASTNode.LABELED_STATEMENT)
			return false;
		
		if (node.getParent().getNodeType() == ASTNode.CONTINUE_STATEMENT)
			return false;
		
		if (node.getParent().getNodeType() == ASTNode.BREAK_STATEMENT)
			return false;
			
		//ld: if the qn belongs to a throw decl. of a method decl. ...
		if (node.getParent().getNodeType() == ASTNode.METHOD_DECLARATION)
			return false;
		
		generatePackageFactIfNecessary(node.resolveBinding());
		
		if (node.resolveBinding().getKind() == IBinding.TYPE){
			handleSelectsIdents(node, null);
			return false;
		}

		
		if (node.resolveBinding().getKind() == IBinding.VARIABLE) {
			IVariableBinding vb = (IVariableBinding) node.resolveBinding();
			
			if (!vb.isField()){
				if ((node.getParent().getParent().getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT ||
					 node.getParent().getParent().getNodeType() == ASTNode.VARIABLE_DECLARATION_EXPRESSION) &&
					 	((VariableDeclarationFragment)node.getParent()).getName() == node)
						return false;
				String[] args = {			
						quote(node.getIdentifier()),
						idResolver.getID(node.resolveBinding())
				};
				createBodyFact(node, "identT", args);
			} else {
				
				if (node.getParent().getNodeType() == ASTNode.FIELD_ACCESS &&
					 	((FieldAccess)node.getParent()).getName() == node)
					return false;
				if (node.getParent().getNodeType() == ASTNode.SUPER_FIELD_ACCESS&&
					 	((SuperFieldAccess)node.getParent()).getName() == node)
					return false;
				if (node.getParent().getParent().getNodeType() == ASTNode.FIELD_DECLARATION &&
				 	((VariableDeclarationFragment)node.getParent()).getName() == node)
					return false;
				//ld: just a note: this will called only for IMPLICIT field access. in particular, it won't be called for constructs like
				// this.var or super.var
				generateGetFieldIfNodeIsFieldAccess(node, node.getIdentifier());
			}
		}
		
		return false;
	}
	
	/**
	 * @param node
	 * @return
	 */
	private boolean generateGetFieldIfNodeIsFieldAccess(Name node, String name) {
		//@TODO: ld: i assume this is correct?
		if(node.resolveBinding()==null) 
			return false;
		
		if (node.resolveBinding().getKind() == IBinding.VARIABLE
				&& ((IVariableBinding)node.resolveBinding()).isField()) {
			//IVariableBinding variable = (IVariableBinding)node.resolveBinding();
			String[] args =
				new String[] {
					node.isSimpleName() ? quote("null") : idResolver.getID(((QualifiedName)node).getQualifier()), 
					quote(name),
					idResolver.getID(node.resolveBinding())};
			//@TODO: ld: a method starting with the "is"-prefix should not have side effects like the one below!
			createBodyFact(node, "getFieldT", args);
			
			
			if (!node.isSimpleName()){
				Name q = ((QualifiedName)node).getQualifier();
				String atomName;
				if (q.isSimpleName())
					atomName = ((SimpleName)q).getIdentifier();
				else
					atomName = ((QualifiedName)q).getName().getIdentifier().toString();
				if (!generateGetFieldIfNodeIsFieldAccess(q, atomName))
					handleSelectsIdents(q, null);
			}
				
			
			return true;
		} 
		return false;
	}
   /**
    * not implemented
    */
	public boolean visit(SimpleType node) {
		return false;
	}
	
	/**
	 * Generates prolog facts of type literalT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LiteralT">
	 * literalT<a>}
	 * 
	 * @see 
	 * FactGeneratorCodeTest#testStringLiteral()
	 */
	public boolean visit(StringLiteral node) {
		String type = typeResolver.getTypeTerm(node);
		//ld: we don't care about the charValue anymore, since we can
		//get the literal text "literaly" from the source code. 
		//char cvalue = node.charValue();
		String contents = getTextForNode(node).substring(1,node.getLength()-1);
		//ld: carefull: we have to escape escape chars, like a backslash
				
		String value = contents.replaceAll("\\\\","\\\\\\\\");
		value = value.replaceAll("'","\\\\'");
		String[] args = new String[] {  type, quote(value) };
		createBodyFact(node, "literalT", args);
		return false;
		
//		String s = node.getLiteralValue();
//		String[] args;
//		/* TODO: TEST this, it is different from the old, working JTransformer */
//		
//		s = s.replaceAll("\\\\", "\\\\\\\\");
//		s = s.replaceAll("\"", "\\\\\""); 
//		s = s.replaceAll("\n", "\\\\n"); // im alten JTransformer: \\\\\n 
//		s = s.replaceAll("\t", "\\\\t"); // im alten JTransformer: \\\\\n 
//		s = s.replaceAll("\r", "\\\\r"); // im alten JTransformer: \\\\\n 
//		s = s.replaceAll("'", "\\\\'"); // im alten auch ein \ mehr
//		
//		args =
//			new String[] {
//				idResolver.getID(node),
//				idResolver.getID(node.getParent()),
//				idResolver.getID(getEnclosingNode(node)),
//				typeResolver.getTypeTerm(node),
//				quote(s) };
//
//		writer.writeFact("literalT", args);
//
//		return true;
	}
	/**
	 * This method has been eaten by Block, because the node should contain
	 * an execT. Normally, you would expect an applyT generated here, but
	 * since the Eclipse AST does not contain an ExpressionStatement wrapper,
	 * it is not generated, confusing the Prolog System (the output lacks a
	 * semicolon). The fact that the parent (a Block) already has an ID for the
	 * applyT, and the children (the Arguments) also do, make it impossible to
	 * locally handle this problem. Therefore, the generation of the execT and
	 * applyT have been put into the Block node. (See comment there)
	 */
	public boolean visit(SuperConstructorInvocation node) {
		return true;
	}
	
	/**
	 * Generates prolog facts of type getFieldT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=GetFieldT">
	 * getFieldT<a>}
	 * @see FactGeneratorMethodBodyTest#testVisitSuperFieldAccess()
	 */
	public boolean visit(SuperFieldAccess node) {
	    String selectedFrom = null;
		
		if (node.getQualifier() == null){
		    
	        TypeDeclaration ultimateAncestor = getUltimateAncestor(node);
	        Name superclassName = ultimateAncestor.getSuperclass();
	        String superClassID = superclassName == null ? idResolver
                    .getJavaLangObjectID() : idResolver.getID(superclassName
                    .resolveTypeBinding());
	        
	        
			selectedFrom = idResolver.getID();
			String [] identArgs = {
					selectedFrom,
					idResolver.getID(node),
					idResolver.getID(getEnclosingNode(node)),
					"'super'",
					superClassID		
			};
			writer.writeFact("identT", identArgs);
			writer.writeFact("slT", new String [] {
					idResolver.getID(node),
					Integer.toString(node.getStartPosition()),
					Integer.toString(node.getLength())
			});
		} else {
		    ITypeBinding superclass = node.getQualifier().resolveTypeBinding().getSuperclass();
		    String superClassID = idResolver.getID(superclass);
			selectedFrom = idResolver.getID();
			String[] selectArgs = {
					selectedFrom,
					idResolver.getID(node),
					idResolver.getID(getEnclosingNode(node)),
					"'super'",
					idResolver.getID(node.getQualifier()),
					superClassID
			};
			writer.writeFact("selectT", selectArgs);
			writer.writeFact("slT", new String [] {
					idResolver.getID(node),
					Integer.toString(node.getStartPosition()),
					Integer.toString(node.getLength())
			});
			handleSelectsIdents(node.getQualifier(), selectedFrom);
		}
	
	
		String[] args =
			new String[] {
				selectedFrom,
				quote(node.getName().getIdentifier()),
				idResolver.getID(node.resolveFieldBinding())
				};
		
		
		createBodyFact(node, "getFieldT", args);

		/*XXX ld's 1st try:do we realy need to dig deeper here?
	 	*ld:yes we need to dig deeper: we need to create an identT for the 'super' 
	 	* expression.
	 	*/	
		
		return true; 
	}
	/**
     * @param node
     */
    private void createSuperIdentTOrSelectT(SuperFieldAccess node) {
        
        
        String args[];
		String type;
		if (node.getQualifier() == null){
			args = new String[] {
				"'super'",				
				idResolver.getID(getUltimateAncestor(node).getSuperclass().resolveTypeBinding())};
			type = "identT";
		} else {
			args = new String[] {
					"super",
					idResolver.getID(node.getQualifier()),
					idResolver.getID(node.getQualifier().resolveTypeBinding())
			};
			type = "selectT";
		}

		createBodyFact(node, type, args);
    }

    /**
	 * Generates prolog facts of type applyT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ApplyT">
	 * applyT<a>}
	 * @see FactGeneratorMethodBodyTest#testVisitSuperMethodInvocation()
   	 */
	public boolean visit(SuperMethodInvocation node) {
		String selectedFrom = null;
				
		
		if (node.getQualifier() == null){
		    TypeDeclaration ultimateAncestor = getUltimateAncestor(node);
	        Name superclassName = ultimateAncestor.getSuperclass();
	        String superClassID = superclassName == null ? idResolver
                    .getJavaLangObjectID() : idResolver.getID(superclassName
                    .resolveTypeBinding());
			
			selectedFrom = idResolver.getID();
			String [] identArgs = {
					selectedFrom,
					idResolver.getID(node),
					idResolver.getID(getEnclosingNode(node)),
					"'super'",
					superClassID		
			};
			writer.writeFact("identT", identArgs);
			writer.writeFact("slT", new String [] {
					idResolver.getID(node),
					Integer.toString(node.getStartPosition()),
					Integer.toString(node.getLength())
			});
		} else {
		    ITypeBinding superclass = node.getQualifier().resolveTypeBinding().getSuperclass();
		    String superClassID = idResolver.getID(superclass);
			selectedFrom = idResolver.getID();
			String[] selectArgs = {
					selectedFrom,
					idResolver.getID(node),
					idResolver.getID(getEnclosingNode(node)),
					"'super'",
					idResolver.getID(node.getQualifier()),
					superClassID
			};
			writer.writeFact("selectT", selectArgs);
			writer.writeFact("slT", new String [] {
					idResolver.getID(node),
					Integer.toString(node.getStartPosition()),
					Integer.toString(node.getLength())
			});
			handleSelectsIdents(node.getQualifier(), selectedFrom);
		}
		
		String[] args =
			new String[] {
				selectedFrom,
				quote(node.getName().getIdentifier()),
				idResolver.getIDs(node.arguments()),
				idResolver.getID(node.resolveMethodBinding())
				};
		createBodyFact(node, "applyT", args);
		
		return true;
	}
	

	/**
	 * Generates prolog facts of type caseT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=CaseT">
	 * caseT<a>}
	 * @see  FactGeneratorTest3b#testSwitch()
	*/
	public boolean visit(SwitchCase node) {
		String [] args = new String[] {
				idResolver.getID(node.getExpression())
		};
		
		createBodyFact(node, "caseT", args);
		
		
		return true;
	}
	/**
	 * Generates prolog facts of type swichT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=SwitchT">
	 * switchT<a>}
	 * @see  FactGeneratorTest3b#testSwitch()
	 */
	public boolean visit(SwitchStatement node) {
		String [] args = new String [] {
				idResolver.getID(node.getExpression()),
				idResolver.getIDs(expandList(node.statements().iterator()))
		};
		
		createBodyFact(node, "switchT", args);
		
		return true;
	}
	/** 
	 * Generates prolog facts of type synchronizedT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=SynchronisedT">
	 * synchronizedT<a>}
	 * @see FactGeneratorTest3#testSynchronizedStatement() 
	 */
	public boolean visit(SynchronizedStatement node) {
		String args[] =
			new String[] {
				idResolver.getID(node.getExpression()),
				idResolver.getID(node.getBody())};

		createBodyFact(node, "synchronizedT", args);
		return true;
	}
	/**
	 * Generates prolog facts of type identT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=IdentT">
	 * identT<a>}
	 * @see FactGeneratorMethodBodyTest#testVisitThisExpression()
	 * @see FactGeneratorMethodBodyTest#testVisitThisExpression_withExpr()
   	 */
	public boolean visit(ThisExpression node) {
		String args[];
		String type;
		if (node.getQualifier() == null){
			args = new String[] {
				"'this'",
				//idResolver.getID(getUltimateAncestor(node))};
				idResolver.getID(node.resolveTypeBinding())};
			type = "identT";
		} else {
			args = new String[] {
					"this",
					idResolver.getID(node.getQualifier()),
					idResolver.getID(node.getQualifier().resolveTypeBinding())
			};
			type = "selectT";
		}

		createBodyFact(node, type, args);
		return true;
	}
	/**
	 * Generates prolog facts of type throwT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ThrowT">
	 * throwT<a>}
	 * @see FactGeneratorTest3#testThrowStatement()
   	 */
	public boolean visit(ThrowStatement node) {
		String [] args = new String[] {
			idResolver.getID(node.getExpression()) };
			
		createBodyFact(node,"throwT", args);
	
		return true;
	}
	/**
	 * Generates prolog facts of type tryT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=TryT">
	 * tryT<a>}
	 * @see FactGeneratorTest3#testTryCatch()
   	 */
	public boolean visit(TryStatement node) {
		String args[] =
			new String[] {
				idResolver.getID(node.getBody()),
				idResolver.getIDs(node.catchClauses()),
				node.getFinally() == null
					? "'null'"
					: idResolver.getID(node.getFinally())};

		createBodyFact(node, "tryT", args);

		return true;
	}

	/** 
	 * Generates prolog facts of type: classDefT, interfaceT, extendsT, implementsT. 
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ClassDefT">
	 * classDefT<a>} 
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=InterfaceT">
	 * interfaceT<a>}  (if the node representes an interface declaration)
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ExtendsT">
	 * extendsT<a>}
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ImplementsT">
	 * implementsT<a>}
	 * @see FactGeneratorInterfaceTest#testTypeDeclaration()
	 */
	public boolean visit(TypeDeclaration node) {
	    String name = quote(node.getName());
	    if(name.equals("'Subroutine'")){
	        Debug.debug("debug");
	    }
		String id = idResolver.getID(node);
		String parentId;
		if (node.getParent().getNodeType() == ASTNode.COMPILATION_UNIT)
			parentId =
				idResolver.getID(
					((CompilationUnit) node.getParent()).getPackage());
		else
			parentId = idResolver.getID(node.getParent());
		

		Iterator bodyIterator = node.bodyDeclarations().iterator();
		ArrayList expandedList = expandList(bodyIterator);

		String prologList = idResolver.getIDs(expandedList);

			
		boolean foundConstructor = false;
		
		for (Iterator i = node.bodyDeclarations().iterator(); i.hasNext();){
			BodyDeclaration bdd = (BodyDeclaration) i.next();
			if (bdd.getNodeType() == ASTNode.METHOD_DECLARATION){
				MethodDeclaration md = (MethodDeclaration) bdd;
				if (md.isConstructor()){
					foundConstructor = true;
					break;
				}
			}
		}
		
		// StS: We do not want a default constructor for an interface.
		if (!foundConstructor && !node.isInterface()){
			//System.out.println("No constructor found: " + prologList);
			String synthConstrId=createSynteticConstructor(node);
			
			prologList = prologList.substring(1);
			
			String prefix = "[" + synthConstrId;
			if (prologList.equals("]"))
				prologList = prefix + prologList;
			else
				prologList = prefix + ", " + prologList;
		}

		

		String[] args = new String[] { id, parentId, name, prologList };
		writer.addIndention();

		writer.writeFact("classDefT", args);
		if (node.isInterface())
			writer.writeFact("interfaceT", new String[] { id });
		writeModifiers(node, node.getModifiers());
		writer.writeFact("slT", new String [] {
				idResolver.getID(node),
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});
		
		Name sc = node.getSuperclass();
		String superClass;
		if (sc != null) 
			superClass = idResolver.getID(sc.resolveBinding());
		else
			/* YUCK! There has to be a better way of doing this
			 * StS
			 */
			superClass = idResolver.getJavaLangObjectID();
			
		writer.writeFact("extendsT", new String [] {id, superClass});
			
				
		Iterator implementsIterator = node.superInterfaces().iterator();
		
		for (; implementsIterator.hasNext(); ){
			Name n = (Name) implementsIterator.next();
			String [] arg = new String [] { id, idResolver.getID(n.resolveBinding()) };
			writer.writeFact("implementsT", arg);
		}
		
		if (!node.isLocalTypeDeclaration()){
			syntheticConstructorIds = new Hashtable();
		}
		
		return true;
	}
	
	/**
	 * @param bodyIterator
	 * @return
	 */
	private ArrayList expandList(Iterator bodyIterator) {
		ArrayList expandedList = new ArrayList();

		for (; bodyIterator.hasNext();) {
			ASTNode astNode = (ASTNode) bodyIterator.next();
			List fragments = getFragments(astNode);
			if (fragments != null) {
				Iterator fieldIterator = fragments.iterator();
				for (; fieldIterator.hasNext();)
					expandedList.add(fieldIterator.next());
			} else
				expandedList.add(astNode);
		}
		return expandedList;
	}

	/**
	 * @param astNode
	 * @return
	 */
	private List getFragments(ASTNode astNode) {
		switch (astNode.getNodeType()) {
			case ASTNode.VARIABLE_DECLARATION_EXPRESSION :
				return ((VariableDeclarationExpression) astNode).fragments();
			case ASTNode.VARIABLE_DECLARATION_STATEMENT :
				return ((VariableDeclarationStatement) astNode).fragments();
			case ASTNode.FIELD_DECLARATION :
				return ((FieldDeclaration) astNode).fragments();
			default :
				return null;
		}
	}
  /**
   * not implemented
   */
	public boolean visit(TypeDeclarationStatement node) {
		String[] args =
			new String[] {
				idResolver.getID(node.getTypeDeclaration())};

		createBodyFact(node, "execT", args);

		return true;
	}
	/**
	 * Generates prolog facts of type literalT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LiteralT">
	 * literalT<a>}
	 * @see FactGeneratorMethodBodyTest#testVisitTypeLiteral()
   	 */
	public boolean _visit(TypeLiteral node) {
		String args[] =
			new String[] {
				"type(class, " + idResolver.getJavaLangClassID() +", 0)",
				typeResolver.getTypeTerm(node.getType())
		};

		createBodyFact(node, "literalT", args);

		return true;
		
	}
	
	/**
	 * Generates prolog facts of type literalT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LiteralT">
8	 * literalT<a>}
	 * @see FactGeneratorMethodBodyTest#testVisitTypeLiteral()
   	 */
	public boolean visit(TypeLiteral node) {
		String identId = idResolver.getID();
		String args[] =
			new String[] {
				"class", identId, "type(class, " + idResolver.getJavaLangClassID() +", 0)"
		};

		createBodyFact(node, "selectT", args);
		writer.writeFact("identT", new String [] {
				identId,
				idResolver.getID(node),
				idResolver.getID(node.getParent()),
				quote(node.getType().resolveBinding().getQualifiedName()),
				typeResolver.getTypeTerm(node.getType())
				
		});

		return true;
		
	}
	/**
	 * Generates prolog facts of type paramDefT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=ParamDefT">
	 * paramDef<a>}
	 * @see FactGeneratorMethodBodyTest#testVisitSingelVariableDeclaration_methodArg()
	 * @see FactGeneratorMethodBodyTest#testVisitSingelVariableDeclaration_catchArg()
	 */
	public boolean visit(SingleVariableDeclaration node) {
		/*
		 * if i understood the javadoc correctly, no matter if we have a
		 * fragmented or an single variable declaration, this is always added!
		 */
		if (node.getParent().getNodeType() == ASTNode.METHOD_DECLARATION) {
			
			String[] args =
				new String[] {
					idResolver.getID(node),
					idResolver.getID(node.getParent()),
					typeResolver.getTypeTerm(node.resolveBinding().getType()), // FIXME: resolve binding is only neces. for native methods 
					quote(node.getName().getIdentifier())};
			writer.writeFact("paramDefT", args);
			writer.writeFact("slT", new String [] {
					idResolver.getID(node),
					Integer.toString(node.getStartPosition()),
					Integer.toString(node.getLength())
			});
		}
		else if (node.getParent().getNodeType() == ASTNode.CATCH_CLAUSE) {
			String[] args =
				new String[] {
					idResolver.getID(node),
					idResolver.getID(node.getParent()),
					typeResolver.getTypeTerm(node.resolveBinding().getType()),
					quote(node.getName().getIdentifier())};
			writer.writeFact("paramDefT", args);
			writer.writeFact("slT", new String [] {
					idResolver.getID(node),
					Integer.toString(node.getStartPosition()),
					Integer.toString(node.getLength())
			});
		}
		else {
			throw new IllegalArgumentException("SingleVariableDeclaration ("+node+") has a parent of unexpected type:"
					+"\n"+node.getParent()+"\n"
					+"Maybe the implementation is inclomplete?");
		}
		writeModifiers(node, node.getModifiers());
		return false;
	}
	
	/**
	 * Generates prolog facts of type localDefT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LocalDefT">
	 * localDefT<a>}
	 * <p>
	 * Used in the initializer part of the for statement, and only there
	 * @see FactGeneratorCodeTest#testLocalVarT()
	 */
	
	public boolean visit(VariableDeclarationExpression node) {
		List fragments = node.fragments();
		int mods = node.getModifiers();
		
		
		for (Iterator i = fragments.iterator(); i.hasNext();) {
			VariableDeclarationFragment n =
				(VariableDeclarationFragment) i.next();
			String[] args =
				new String[] {
					idResolver.getID(n),
					idResolver.getID(node.getParent()),
					idResolver.getID(getEnclosingNode(n)),
					typeResolver.getTypeTerm(n.resolveBinding().getType()),
					"'" + n.getName().getIdentifier() + "'",
					idResolver.getID(n.getInitializer())};

			writer.writeFact("localDefT", args);
			writer.writeFact("slT", new String [] {
					idResolver.getID(n),
					Integer.toString(n.getStartPosition()),
					Integer.toString(n.getLength())
			});
			
			writeModifiers(n, mods);

		}
		return true;
	}
	/**
	 * Generates prolog facts of type localDefT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=LocalDefT">
	 * localDefT<a>}
	 * @see FactGeneratorCodeTest#testLocalVarT()
   	 */
	public boolean visit(VariableDeclarationStatement node) {
		List fragments = node.fragments();
		int mods = node.getModifiers();
		
		for (int i = 0; i < fragments.size(); i++) {
			VariableDeclarationFragment n =
				(VariableDeclarationFragment) fragments.get(i);
			
//			if(n.getName().toString().equals("solutions"))
//				System.out.println("DEBUG");

			VariableDeclarationFragment theNode = n;
            ASTNode theParent = theNode.getParent().getParent();
            ASTNode theEnc = getEnclosingNode(theNode);
            ITypeBinding theType = theNode.resolveBinding().getType();
            String theName = "'" + theNode.getName().getIdentifier() + "'";
            if(theName.equals("'sub'")){
                Debug.debug("debug");
            }
            Expression theInitializer = theNode.getInitializer();
            String[] args =
				new String[] {
					idResolver.getID(theNode),
					idResolver.getID(theParent),
					idResolver.getID(theEnc),
					typeResolver.getTypeTerm(theType),
					theName,
					idResolver.getID(theInitializer)};

			writer.writeFact("localDefT", args);
			writer.writeFact("slT", new String [] {
					idResolver.getID(theNode),
					Integer.toString(theNode.getStartPosition()),
					Integer.toString(theNode.getLength())
			});
			writeModifiers(theNode, mods);

		}
		return true;
	}
	/**
	   * not implemented
	   */
	public boolean visit(VariableDeclarationFragment node) {
		/* NOT IMPLEMENTED, PARENT */
		return true;
	}
	/**
	 * Generates prolog facts of type whileLoopT.
	 * <p>
	 * {@link <a href="http://roots.iai.uni-bonn.de/lehre/xp2004a1/Wiki.jsp?page=WhileLoopT">
	 * whileLoopT<a>}
	 * @see FactGeneratorTest3#testWhileStatement()
   	 */
	public boolean visit(WhileStatement node) {
		String[] args =
			new String[] {
				idResolver.getID(node.getExpression()),
				idResolver.getID(node.getBody())};

		createBodyFact(node, "whileLoopT", args);

		return true;
	}

	public void endVisit(LabeledStatement node) {
		labels.remove(node.getLabel().getIdentifier());
	}
	public void endVisit(TypeDeclaration t) {
		writer.reduceIndention();
	}

	public void endVisit(Block b) {
		writer.reduceIndention();
	}

	private ASTNode getEnclosingNode(ASTNode a) {
		if (a == null)
			throw new RuntimeException("enclosing node is null");
		
		if (a instanceof MethodDeclaration
		 || (a instanceof VariableDeclarationFragment && a.getParent() instanceof FieldDeclaration)
		 || a instanceof FieldDeclaration
		 || a instanceof Initializer)
			return a;
		return getEnclosingNode(a.getParent());
	}

	private void writeModifiers(ASTNode node, int modifiers) {
		if (Modifier.isPublic(modifiers))
			writeModifier(node, "public");
		if (Modifier.isPrivate(modifiers))
			writeModifier(node, "private");
		if (Modifier.isStatic(modifiers))
			writeModifier(node, "static");
		if (Modifier.isAbstract(modifiers))
			writeModifier(node, "abstract");
		if (Modifier.isFinal(modifiers))
			writeModifier(node, "final");
		if (Modifier.isProtected(modifiers))
			writeModifier(node, "protected");
		if (Modifier.isNative(modifiers))
			writeModifier(node, "native");
		if (Modifier.isSynchronized(modifiers))
			writeModifier(node, "synchronized");
		if (Modifier.isTransient(modifiers))
			writeModifier(node, "transient");
		if (Modifier.isVolatile(modifiers))
			writeModifier(node, "volatile");
		if (Modifier.isStrictfp(modifiers))
			writeModifier(node, "strictfp");
	}

	private void writeModifier(ASTNode node, String modifier) {
		writer.writeFact(
			"modifierT",
			new String[] { idResolver.getID(node), "'" + modifier + "'" });
	}
	private String quote(String str) {
		return "'" + str + "'";
	}
	private String quote(SimpleName name) {
		return quote(name.toString());
	}

	private ASTNode getEnclosingIterator(ASTNode node) {
		
		while (!(node instanceof ForStatement)
			&& !(node instanceof DoStatement)
			&& !(node instanceof SwitchStatement)
			&& !(node instanceof WhileStatement)){
			if (node==null){
				throw new IllegalArgumentException("node should _NOT_ be null, huh?");
			}
			node = node.getParent();
		}
		return node;
	}

	private TypeDeclaration getUltimateAncestor(ASTNode node) {
		while (!(node instanceof TypeDeclaration))
			node = node.getParent();
		return (TypeDeclaration) node;
	}
	/**
	 * returns an empty list
	 */
	public String getEmptyList() {
		return "[]";
	}
	
	private void handleSelectsIdents(Name name, String selectedFrom) {
		String [] args;
		
		/* StS - Not sure if i need to add an slT Fact here as well */
		
		generatePackageFactIfNecessary(name.resolveBinding());
		
		if (selectedFrom == null)
			selectedFrom = idResolver.getID(name.getParent());
		
		if (name instanceof SimpleName){
			args = new String [] {
					idResolver.getID(name),
					selectedFrom,
					idResolver.getID(getEnclosingNode(name)),
					quote((SimpleName)name),
					idResolver.getID(name.resolveBinding())
			};
			writer.writeFact("identT", args);
		} else {
			QualifiedName qn = (QualifiedName) name;
			args = new String [] {
					idResolver.getID(name),
					selectedFrom,
					idResolver.getID(getEnclosingNode(name)),
					"'" + qn.getName().toString() + "'",
					idResolver.getID(qn.getQualifier()),
					idResolver.getID(qn.resolveBinding())
			};
			writer.writeFact("selectT", args);
			handleSelectsIdents(qn.getQualifier(), null);
		}
	}
	
	/* IT IS DEAD! YES!
	 * 
	 * private void writeMemberArrays(ArrayInitializer initializer, ArrayType type) {
		for (Iterator i = initializer.expressions().iterator(); i.hasNext(); ){
			ArrayInitializer ai = (ArrayInitializer) i.next();
			
			String myID = idResolver.getID(ai);
			String pID = idResolver.getID(ai.getParent());
			String blockID = idResolver.getID(getEnclosingNode(ai));
			String dims = getEmptyList();
			String typeString = typeResolver.getTypeTerm(type);
			
			StringBuffer initBuffer = new StringBuffer();
			boolean first = true;
			
			initBuffer.append("[");
			
			for (Iterator it = ai.expressions().iterator(); it.hasNext();) {
				ASTNode e = (ASTNode) it.next();
				
				if (!first)
					initBuffer.append(", ");
				else
					first = false;
				
				initBuffer.append(idResolver.getID(e));
			}
			
			initBuffer.append("]");
			
			String [] args = new String [] {
					myID,
					pID,
					blockID,
					dims,
					initBuffer.toString(),
					typeString
			};
			
			writer.writeFact("newArrayT", args);
			if (type.getComponentType() instanceof ArrayType)
				writeMemberArrays(ai, (ArrayType) type.getComponentType());
		}						  
			
	}*/
	
	/**
	 * retrieve the underlying ICompilationUnit.
	 * 
	 * The AST on which this FactGenerator is to operate corresponds to an 
	 * underlying ICompilationUnit instance (not to be confused with the 
	 * CompilationUnit, which is a subclass of ASTNode and represents the 
	 * root of the AST). This method returns this ICompilationUnit, <b>IF</b>
	 * this field was set, i.e. if the right constructor was used to 
	 * instantiate this FactGenerator. Otherwise null is returned.
	 * 
	 * @return Returns the iCompilationUnit or null.
	 */
	public ICompilationUnit getICompilationUnit() {
		return iCompilationUnit;
	}

	/**
	 * @param importName
	 */
	private void generatePackageFactIfNecessary(IBinding binding) {
		if (binding == null){
			Debug.warning("generatePackageFactIfNecessary called on null binding");
			return;
		}
		String fullName = binding.getName();
		if (binding.getKind() == IBinding.PACKAGE && packages.get(fullName) == null) {
			String id = idResolver.getID(binding);
			writer.writeFact("packageT", new String[] { id, quote(fullName) });
			packages.put(fullName,fullName);
		}
	}
	
	private String createSynteticConstructor(TypeDeclaration typeDeclaration) {
		ITypeBinding binding = typeDeclaration.resolveBinding();


        String fqn = idResolver.getSyntheticConstructorID(binding);
		
		String classname = typeDeclaration.resolveBinding().getQualifiedName();
		String bodyID = idResolver.getID();
		
		
		if(typeDeclaration.isLocalTypeDeclaration()) {
			fqn = idResolver.getID(); 
			syntheticConstructorIds.put(binding,fqn);
		} else {
			fqn = fqnManager.transformFQN(fqn);
		}
		
		
		String [] args = {
				fqn, 
				idResolver.getID(typeDeclaration),
				"'<init>'",
				"[]",
				"type(basic, void, 0)",
				"[]",
				bodyID
		};
		
		writer.writeFact("methodDefT", args);
		writer.addIndention();
		
		args = new String [] {
				fqn,
				"public"
		};
		
		writer.writeFact("modifierT", args);
		args[1] = "synthetic";
		writer.writeFact("modifierT", args);
		writer.addIndention();
		
		/*	
		 * :- inTe(blockT(100002, fqn('humptydumpty', '<init>', []), fqn('humptydumpty', '<init>', []), [100004])).
		 * :- inTe(execT(100004, 100002, fqn('humptydumpty', '<init>', []), 100003)).
		 * :- inTe(applyT(100003, 100004, fqn('humptydumpty', '<init>', []), 'null', 'super', [], fqn('java.lang.Object', '<init>', []))).
		 */
		
		String execID = idResolver.getID();
		String applyID = idResolver.getID();
		
		args = new String [] {
				bodyID,
				fqn, 
				fqn,
				"[" + execID + "]"
		};
		
		writer.writeFact("blockT", args);
		
		args = new String [] {
				execID, 
				bodyID,
				fqn,
				applyID
		};
		
		
		Name sname = typeDeclaration.getSuperclass();
		String superc;
		
		if (sname != null){
            superc=idResolver.getSyntheticConstructorID((ITypeBinding) sname.resolveBinding());
			//superc = "fqn('" + ((ITypeBinding)sname.resolveBinding()).getQualifiedName() + "', '<init>', [])";
		} else {
            superc=idResolver.getSyntheticConstructorID(null);
			//superc = "fqn('java.lang.Object', '<init>', [])";
		}
		
		fqnManager.transformFQN(superc);
		
		
		writer.writeFact("execT", args);
		
		args = new String [] {
				applyID, 
				execID,
				fqn,
				"'null'",
				"'super'",
				getEmptyList(),
				superc
		};
		
		writer.writeFact("applyT", args);
		writer.reduceIndention();
		writer.reduceIndention();
		return fqn;
	}
	
	/* StS: Merged into createBodyFact and visit() 
	public void postVisit(ASTNode node) {
		
	
		writer.writeFact("slT", 
				new String[] {
				idResolver.getID(node),
				""+node.getStartPosition(), 
				""+node.getLength()
		});
	}*/
	
	private void createBodyFact(ASTNode node, String name, String [] args){
		String [] toPass = new String [args.length + 3];
		
		toPass[0] = idResolver.getID(node);
		toPass[1] = idResolver.getID(node.getParent());
		toPass[2] = idResolver.getID(getEnclosingNode(node));
		
		System.arraycopy(args, 0, toPass, 3, args.length);
		writer.writeFact(name, toPass);
		writer.writeFact("slT", new String [] {
				toPass[0],
				Integer.toString(node.getStartPosition()),
				Integer.toString(node.getLength())
		});
		GenericAnnotation annotation = getCorrespondingGenericAnnotation(node);
		if(annotation != null && !name.equals("execT")) {
			writer.writeFact("annotationT",new String[]{
					toPass[0], annotation.getPredicate()
			});
		}
	}

}


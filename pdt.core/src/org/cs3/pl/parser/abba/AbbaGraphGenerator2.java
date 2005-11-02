package org.cs3.pl.parser.abba;

import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Vector;

import org.cs3.pl.parser.internal.term.ASTCompilationUnit;
import org.cs3.pl.parser.internal.term.ASTCompoundTerm;
import org.cs3.pl.parser.internal.term.ASTMember;
import org.cs3.pl.parser.internal.term.DefaultPrologTermParserVisitor;
import org.cs3.pl.parser.internal.term.SimpleNode;


/* TODO: arguments of clause heads or rules are currently only processed for
 * ct rules. 
 * 
 */

public class AbbaGraphGenerator2 extends DefaultPrologTermParserVisitor {

	

	/*
	 * if false (the default), arguements to goals (that are not considered
	 * meta-calls) are treated like atoms, i.e. they will not be traversed.
	 */
	private final static boolean TRAVERSE_LITERALS = true;

	private static final String INTE = "in_te";

	/*
	 * responsible for actualy writing the abba facts
	 */
	private NodeWriterStrategy writerStrategy;

	/*
	 * responsible for generating abba node IDs
	 */
	private IDGeneratorStrategy idStrategy;
	
	/*
	 * maps module-qualified predicate names (<module>:<functor>/<arity>) to
	 * abba node IDs
	 */
	private Map predicates = new HashMap();

	/*
	 * the module defined in the examined compilation unit, if any.
	 */
	private String cuModule;

	/*
	 * the filename of the current compilation unit
	 */
	private String cuFileName;

	/*
	 * a data structure describing which argument positions for a term of a
	 * given signature are potentialy evaluated (expanded) by the prolog
	 * interpreter. In adition, the struct information if such a term should be
	 * considered as a literal itself (e.g. forall/2). This is of course a very
	 * simplistic aproach. It is intended as an ad-hoc solution and should be
	 * replaced by a proper call graph analysis later.
	 */
	private static class GoalInfo {
		/*
		 * name/arity of the goal
		 */
		final String functor;

		/*
		 * for meta predicates, this array contains indizes of all arguments
		 * that are goals.
		 * 
		 */
		final BitSet activePositions;

		/*
		 * if true, the goal is considered a literal. otherwise the goal is
		 * treated as a transparent meta-call, that is, no abba node is created
		 * for it.
		 */
		final boolean isLiteral;

		public GoalInfo(String functor, int[] positions, boolean literal) {
			super();
			activePositions = new BitSet(8);
			for (int i = 0; i < positions.length; i++) {
				activePositions.set(positions[i]);
			}
			this.functor = functor;
			isLiteral = literal;
		}

	}

	/*
	 * Data structure that is passed UP the tree. Contains information on the
	 * generated child note (abba id and transformed term)
	 *
	 */
	private static class Child{
		private StringBuffer term=new StringBuffer();
		private String childID;
		public Child(String id,String name,String[] args){
			childID=id;
			term.append(INTE);
			term.append('(');
			term.append(childID);
			term.append(',');
			term.append(name);			
			append_args(args);			
			term.append(')');
			
		}
		private void append_args(String[] args) {
			if(args.length==0){
				return;
			}
			term.append('(');
			for (int i = 0; i < args.length; i++) {				
				if(i>0){
					term.append(',');		
				}
				term.append(args[i]);				
			}
			term.append(')');
		}
		
		public Child(String id,String name,Child[] args){
			childID=id;
			term.append(INTE);
			term.append('(');
			term.append(childID);
			term.append(',');
			term.append(name);			
			append_args(args);			
			term.append(')');
			
		}
		private void append_args(Child[] args) {
			if(args.length==0){
				return;
			}
			term.append('(');
			for (int i = 0; i < args.length; i++) {				
				if(i>0){
					term.append(',');		
				}
				term.append(args[i].term);				
			}
			term.append(')');
		}
		
	}
	
	/*
	 * Data structure that is passed DOWN the tree and carries information on
	 * the context in which subtrees are to be interpreted.
	 */
	private static class Context {
		
		
		public static final int CLAUSE_HEAD = 0; // within an argument to a fact or
													// clause head

		public static final int CLAUSE_BODY = 1; // within the body of a
													// clause

		public static final int CT_CONDITION = 2; // within condition term of
													// a ct

		public static final int CT_ACTION = 3; // within action term of a ct
		
		public static final int OTHER = -1; //none of the above

		public boolean onCallablePosition = false; // wether parent thinks node
													// is a (sub-)goal

		public int subtreeContext = Context.OTHER; //what subtree lies node in?
		
		/*
		 * during traversal this variable will contain the abba id of the last abba
		 * node that was created. Afaicsatm, this should always point to the parent
		 * of the next node that is to be created.
		 */
		private String parentId;
		
		
		/*
		 * during traversal of a node that is considered to be a clause, this
		 * variable will contain the corresponding abba id.
		 * during traversal of a node that is considered to be a ct, this variable
		 * will contain the coresponding abba id.
		 */
		private String memberId;
		public Context(int subtreeContext){
			this.subtreeContext =subtreeContext;
		}

		public Context(int subtreeContext, String parentId, String memberId) {
			super();	
			this.subtreeContext = subtreeContext;
			this.parentId = parentId;
			this.memberId = memberId;
		}

	}

	/*
	 * maps predicates (<fuctor>:<arity>) to GoalInfo Objects (see above)
	 * 
	 */
	private Map goalinfo = new HashMap();

		
	/*
	 * i cannot exactly remember how stull is passed up visitor implementation
	 * so i use my own stack.
	 */
	private Stack stack=new Stack();
	

	public AbbaGraphGenerator2(NodeWriterStrategy writerStrategy,
			IDGeneratorStrategy idStrategy) {

		this.writerStrategy = writerStrategy;
		this.idStrategy = idStrategy;
		goalinfo.put("','/2", new GoalInfo("','/2", new int[] { 0, 1 }, false));
		goalinfo.put("';'/2", new GoalInfo("';'/2", new int[] { 0, 1 }, false));
		goalinfo.put("'|'/2", new GoalInfo("'|'/2", new int[] { 0, 1 }, false));
		goalinfo.put("'->'/2",
				new GoalInfo("'->'/2", new int[] { 0, 1 }, false));
		goalinfo.put("'*->'/2", new GoalInfo("'*->'/2", new int[] { 0, 1 },
				false));
		goalinfo
				.put("'\\+'/1", new GoalInfo("'\\+'/1", new int[] { 0 }, false));
		goalinfo.put("call/1", new GoalInfo("call/1", new int[] { 0 }, false));
		goalinfo.put("not/1", new GoalInfo("not/1", new int[] { 0 }, false));
		goalinfo.put("ignore/1", new GoalInfo("ignore/1", new int[] { 0 },
				false));
		goalinfo.put("call_with_depth_limit/3", new GoalInfo(
				"call_with_depth_limit/3", new int[] { 0 }, true));
		goalinfo.put("call_cleanup/3", new GoalInfo("call_cleanup/3",
				new int[] { 0, 2 }, true));
		goalinfo.put("call_cleanup/2", new GoalInfo("call_cleanup/2",
				new int[] { 0, 1 }, true));
		goalinfo.put("catch/3", new GoalInfo("catch/3", new int[] { 0, 2 },
				true));
		goalinfo.put("block/3", new GoalInfo("block/3", new int[] { 1 }, true));
		goalinfo.put("findall/3",
				new GoalInfo("block/3", new int[] { 1 }, true));
		goalinfo.put("setof/3", new GoalInfo("block/3", new int[] { 1 }, true));
		goalinfo.put("bagof/3", new GoalInfo("bagof/3", new int[] { 1 }, true));
		goalinfo.put("forall/2", new GoalInfo("forall/2", new int[] { 0, 1 },
				true));
	}

	/*
	 * --> entry point <--
	 */
	public Object visit(ASTCompilationUnit node, Object data) {
		this.cuModule = node.getModuleName();
		this.cuFileName = node.getFilename();
		traverseChildren(node, new Context(Context.OTHER,null,null));
		return node;
	}

	/*
	 * traverse a CU member (clause, fact or ct - directives are ignored)
	 * 
	 * for any non-directive rule, we first look at the head and generate clause
	 * and predicate nodes (latter only if neccessary)
	 * 
	 * depending on what kind of member we are looking at, we then identify a
	 * set of subtrees that are of interest for us. directives are ignored as
	 * mentioned above non-ct facts currently we do not further look into the
	 * argument terms. So facts are just terminal nodes. ct facts (see
	 * visitCtTerm(..) below we create a ct root node and respective sym tab
	 * entries The subtrees corresponding to the condition and action arguments
	 * are traversed and connected to the ct node. rules the body term is
	 * traversed and connected to the clause node.
	 * 
	 * 
	 * 
	 */
	public Object visit(ASTMember node, Object data) {
		if(node.isDirective()){
			return node;
		}
		if(node.isFact()){
			if("ct/3".equals(node.getHeadLiteral().getFunctor())){
				visitCt((ASTCompoundTerm) node.getHeadLiteral().toCanonicalTerm(true,true), data);
			}
			else{
				visitFact( node, data);
			}
		} else{
			visitRule( node, data);
		}
		if(stack.size()!=1){
			throw new IllegalStateException("stack should contain exactly ONE element.");
		}
		Child c = (Child) stack.pop();
		writerStrategy.writeAssert(c.term.toString());
		return node;
				
	}

	private Object visitCt(ASTCompoundTerm ct, Object data) {
		
		
		SimpleNode[] args = ct.getArguments();
		SimpleNode condition = args[1];
		SimpleNode action = args[2];
		Context c = new Context(Context.CT_CONDITION);
		c.onCallablePosition=true;
		
		traverse(condition,c);
		Child conditionChild = (Child) stack.pop();
		c.subtreeContext=Context.CT_ACTION;
		traverse(action,c);
		Child actionChild = (Child) stack.pop();
		
		String ctId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CT, ct);
		String ctName = args[0].getSyntheticImage();
		writerStrategy.writeSymTabEntry(ctName, ctId);
		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CT, ctId, ctName);
		SimpleNode original = ct.getOriginal();
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		
		
		
		writerStrategy.writeProperty(ctId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		writerStrategy.writeProperty(ctId, NodeWriterStrategy.PROPERTY_FILE,
				new String[] { "'" + cuFileName + "'" });
		String edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_CT_ACTION, null, ctId, actionChild.childID);
		writerStrategy.writeEdge(edgeId,
				NodeWriterStrategy.EDGE_TYPE_CT_ACTION, null, ctId, actionChild.childID);
		edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_CT_ACTION, null, ctId, conditionChild.childID);
		writerStrategy.writeEdge(edgeId,
				NodeWriterStrategy.EDGE_TYPE_CT_ACTION, null, ctId, conditionChild.childID);
		
		
		Child me = new Child(ctId,"ct",new String[]{ctName,conditionChild.term.toString(),actionChild.term.toString()});
		stack.push(me);
		return ct;
	}

	
	
	private Object visitRule(ASTMember node, Object data) {
		String functor = node.getHeadLiteral().getFunctor();
		String name = node.getHeadLiteral().getPrincipal().getSyntheticImage();
		String moduleQualifiedFunctor = node.getModuleName() + ":" + functor;

		

		Context c = new Context(Context.CLAUSE_HEAD);
		c.onCallablePosition=false;
		
		traverse(node.getHeadLiteral().toCanonicalTerm(true,true),c);
		Child headChild = (Child) stack.pop();
		
		c.subtreeContext=Context.CLAUSE_BODY;
		c.onCallablePosition=true;
		traverse(node.getBody().toCanonicalTerm(true,true),c);
		Child bodyChild = (Child) stack.pop();
		
		
		
		// write clause node	
		String clauseId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CLAUSE,
				node);
		

		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CLAUSE, clauseId,
				functor);
		//connect to predicate
		String predicateId = getPredicateId(moduleQualifiedFunctor);
		
		String edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_CLAUSE, null, predicateId,
				clauseId);

		writerStrategy.writeEdge(edgeId, NodeWriterStrategy.EDGE_TYPE_CLAUSE,
				null, predicateId, clauseId);
		
		//write clause properties
		SimpleNode original = node.getOriginal();
		
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(clauseId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		writerStrategy.writeProperty(clauseId,
				NodeWriterStrategy.PROPERTY_FILE, new String[] { "'"
						+ cuFileName + "'" });
		
		//connect children
		edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_HEAD_LITERAL, null, clauseId, headChild.childID);
		writerStrategy.writeEdge(edgeId,
				NodeWriterStrategy.EDGE_TYPE_HEAD_LITERAL, null, clauseId, headChild.childID);
		edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_BODY_LITERAL, null, clauseId, bodyChild.childID);
		writerStrategy.writeEdge(edgeId,
				NodeWriterStrategy.EDGE_TYPE_BODY_LITERAL, null, clauseId, bodyChild.childID);
		
		//push node
		Child me = new Child(clauseId,":-",new String[]{headChild.term.toString(),bodyChild.term.toString()});
		stack.push(me);
		return node;	
	}

	private Object visitFact(ASTMember node, Object data) {
		String functor = node.getHeadLiteral().getFunctor();
		String name = node.getHeadLiteral().getPrincipal().getSyntheticImage();
		String moduleQualifiedFunctor = node.getModuleName() + ":" + functor;

		Context c = new Context(Context.CLAUSE_HEAD);
		c.onCallablePosition=false;
		SimpleNode head = node.getHeadLiteral().toCanonicalTerm(true,true);
		SimpleNode[] args = null;
		if(head instanceof ASTCompoundTerm){
			args=((ASTCompoundTerm) head).getArguments();	
		}else{
			args=new SimpleNode[0];
		}
		Child[] children = new Child[args.length];
		
		for (int i = 0; i < children.length; i++) {
			traverse(args[i],c);
			children[i]=(Child) stack.pop();
		}
		

		// write clause node	
		String clauseId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CLAUSE,
				node);
		

		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CLAUSE, clauseId,
				functor);
		//connect to predicate
		String predicateId = getPredicateId(moduleQualifiedFunctor);
		
		String edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_CLAUSE, null, predicateId,
				clauseId);

		writerStrategy.writeEdge(edgeId, NodeWriterStrategy.EDGE_TYPE_CLAUSE,
				null, predicateId, clauseId);
		
		//write clause properties
		SimpleNode original = node.getOriginal();
		
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(clauseId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		writerStrategy.writeProperty(clauseId,
				NodeWriterStrategy.PROPERTY_FILE, new String[] { "'"
						+ cuFileName + "'" });
		
		//connect children
		for (int i = 0; i < children.length; i++) {
			edgeId = idStrategy.getEdgeId(
					NodeWriterStrategy.EDGE_TYPE_HEAD_LITERAL, null, clauseId, children[i].childID);
			writerStrategy.writeEdge(edgeId,
					NodeWriterStrategy.EDGE_TYPE_HEAD_LITERAL, null, clauseId, children[i].childID);
			
		}
		
		//push node
		Child me = new Child(clauseId,name,children);
		stack.push(me);
		return node;
	}

	
	public Object visit(ASTCompoundTerm node, Object data) {
		//traverse children
		String functor = node.getFunctor();
		GoalInfo info = (GoalInfo) goalinfo.get(functor);
		SimpleNode[] args = node.getArguments();
		Child[] children = new Child[args.length];
		Context c = (Context)data;
		boolean meCallable = c.onCallablePosition;
		for (int i = 0; i < children.length; i++) {
			c.onCallablePosition=info!=null&&meCallable&&info.activePositions.get(i);
			traverse(args[i],c);
			children[i]=(Child)stack.pop();
		}
		
		
		String name = node.getPrincipal().getSyntheticImage();
//		 write clause node	
		String nodeType=null;
		String edgeType=null;
		switch(c.subtreeContext){
		case Context.CLAUSE_BODY:
			nodeType=NodeWriterStrategy.NODE_TYPE_BODY_LITERAL;
			edgeType=NodeWriterStrategy.EDGE_TYPE_BODY_LITERAL;
			break;
		case Context.CLAUSE_HEAD:
			nodeType=NodeWriterStrategy.NODE_TYPE_HEAD_LITERAL;
			edgeType=NodeWriterStrategy.EDGE_TYPE_HEAD_LITERAL;
			break;
		case Context.CT_CONDITION:
			nodeType=NodeWriterStrategy.NODE_TYPE_CT_CONDITION;
			edgeType=NodeWriterStrategy.EDGE_TYPE_CT_CONDITION;
			break;
		case Context.CT_ACTION:
			nodeType=NodeWriterStrategy.NODE_TYPE_CT_ACTION;
			edgeType=NodeWriterStrategy.EDGE_TYPE_CT_ACTION;
			break;
		default:
			throw new IllegalArgumentException("bad, bad context");
		}
		
		String literalId = idStrategy.getNodeId(nodeType,node);
		

		writerStrategy.writeNode(nodeType, literalId,functor);
		//write literal properties
		SimpleNode original = node.getOriginal();
		
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(literalId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		writerStrategy.writeProperty(literalId,
				NodeWriterStrategy.PROPERTY_FILE, new String[] { "'"
						+ cuFileName + "'" });
		writerStrategy.writeProperty(literalId,
				NodeWriterStrategy.PROPERTY_CALLABLE, new String[] {
						meCallable?"'true'":"'false'" });
		//connect children
		for (int i = 0; i < children.length; i++) {
			String edgeId = idStrategy.getEdgeId(
					edgeType, null, literalId, children[i].childID);
			writerStrategy.writeEdge(edgeId,
					edgeType, null, literalId, children[i].childID);
			
		}
		
		//push node
		Child me = new Child(literalId,name,children);
		stack.push(me);
		return node;
	}

	

	protected Object visitAllOther(SimpleNode node, Object data) {
		String name = node.getPrincipal().getSyntheticImage();
		String nodeType=null;
		
		Context c = (Context) data;
		switch(c.subtreeContext){
		case Context.CLAUSE_BODY:
			nodeType=NodeWriterStrategy.NODE_TYPE_BODY_LITERAL;		
			break;
		case Context.CLAUSE_HEAD:
			nodeType=NodeWriterStrategy.NODE_TYPE_HEAD_LITERAL;		
			break;
		case Context.CT_CONDITION:
			nodeType=NodeWriterStrategy.NODE_TYPE_CT_CONDITION;		
			break;
		case Context.CT_ACTION:
			nodeType=NodeWriterStrategy.NODE_TYPE_CT_ACTION;		
			break;
		default:
			throw new IllegalArgumentException("bad, bad context");
		}
		String literalId = idStrategy.getNodeId(nodeType,node);
		String functor = node.getFunctor();
		
		boolean meCallable=c.onCallablePosition;

		writerStrategy.writeNode(nodeType, literalId,functor);
		//write literal properties
		SimpleNode original = node.getOriginal();
		
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(literalId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		writerStrategy.writeProperty(literalId,
				NodeWriterStrategy.PROPERTY_FILE, new String[] { "'"
						+ cuFileName + "'" });
		writerStrategy.writeProperty(literalId,
				NodeWriterStrategy.PROPERTY_CALLABLE, new String[] {
						meCallable?"'true'":"'false'" });
		
		//push node
		
		Child me = new Child(literalId,name,new String[0]);
		stack.push(me);
		return node;
	}

	/*
	 * lookup a predicate node id by its module-qualified name. If not found,
	 * create a new predicate node and return its id.
	 */
	private String getPredicateId(String moduleQualifiedFunctor) {
		String predicateId = (String) predicates.get(moduleQualifiedFunctor);
		if (predicateId == null) {
			predicateId = idStrategy.getNodeId(
					NodeWriterStrategy.NODE_TYPE_PREDICATE,
					moduleQualifiedFunctor);
			predicates.put(moduleQualifiedFunctor, predicateId);
			writerStrategy
					.writeSymTabEntry(moduleQualifiedFunctor, predicateId);
			writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_PREDICATE,
					predicateId, moduleQualifiedFunctor);
		}
		return predicateId;
	}

}

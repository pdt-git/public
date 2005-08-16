package org.cs3.pl.parser.internal.term;

import java.util.HashMap;

import org.cs3.pl.parser.Problem;
import org.cs3.pl.parser.ProblemCollector;

public class PEFChecker extends DefaultPrologTermParserVisitor {
	private final ProblemCollector problemCollecter;
	private HashMap pefs= new HashMap();

	public PEFChecker(ProblemCollector problemCollecter) {
		this.problemCollecter = problemCollecter;
		initPEFs();
	}
	
	public Object visit(ASTCompilationUnit node, Object data) {
		SimpleNode canonical = node.toCanonicalTerm(true,true);
		return super.visit(canonical, canonical);
	}
	
	public Object visit(ASTCompoundTerm node, Object data) {
		String label = node.getLabel();
		if(pefs.containsKey(label)){
			Integer expected = (Integer) pefs.get(label);
			int arity = node.getOriginal().getArity();
			if(arity!=expected.intValue()){
				Problem p = TermParserUtils.createProblem(node.getOriginal(),"Expected arity for '" + label
	                    + "' is " + expected.intValue() + ", arity found: " + arity
	                    + ".",Problem.WARNING);
				problemCollecter.reportProblem(p);
			}
		}
		return super.visit(node, data);
	}
	
	
	private void initPEFs() {
        pefs.put("fieldDefT", new Integer(5));
        pefs.put("paramDefT", new Integer(4));
        pefs.put("localDefT", new Integer(6));
        pefs.put("methodDefT", new Integer(7));
        pefs.put("classDefT", new Integer(4));
        pefs.put("getFieldT", new Integer(6));
        pefs.put("identT", new Integer(5));
        pefs.put("literalT", new Integer(5));
        pefs.put("execT", new Integer(4));
        pefs.put("operationT", new Integer(6));
        pefs.put("applyT", new Integer(7));
        pefs.put("blockT", new Integer(4));
        pefs.put("selectT", new Integer(6));
        pefs.put("conditionalT", new Integer(6));
        pefs.put("ifT", new Integer(6));
        pefs.put("assignT", new Integer(5));
        pefs.put("importT", new Integer(3));
        pefs.put("newArrayT", new Integer(6));
        pefs.put("toplevelT", new Integer(4));
        pefs.put("newClassT", new Integer(8));
        pefs.put("returnT", new Integer(4));
        pefs.put("switchT", new Integer(5));
        pefs.put("typeCastT", new Integer(5));
        pefs.put("tryT", new Integer(6));
        pefs.put("whileLoopT", new Integer(5));
        pefs.put("continueT", new Integer(5));
        pefs.put("doLoopT", new Integer(5));
        pefs.put("indexedT", new Integer(5));
        pefs.put("throwT", new Integer(4));
        pefs.put("forLoopT", new Integer(7));
        pefs.put("synchronizedT", new Integer(5));
        pefs.put("labelT", new Integer(5));
        pefs.put("breakT", new Integer(5));
        pefs.put("typeTestT", new Integer(5));
        pefs.put("assignopT", new Integer(6));
        pefs.put("caseT", new Integer(4));
        pefs.put("catchT", new Integer(5));
        pefs.put("assertT", new Integer(5));
        pefs.put("modifierT", new Integer(2));
        pefs.put("externT", new Integer(1));
        pefs.put("interfaceT", new Integer(1));
        pefs.put("lastID", new Integer(1));
        pefs.put("packageT", new Integer(2));
        pefs.put("implementsT", new Integer(2));
        pefs.put("extendsT", new Integer(2));
        pefs.put("precedenceT", new Integer(4));
        pefs.put("nopT", new Integer(3));
    }
}

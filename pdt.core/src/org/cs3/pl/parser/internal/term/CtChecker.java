package org.cs3.pl.parser.internal.term;

import org.cs3.pl.parser.Problem;
import org.cs3.pl.parser.ProblemCollector;
import org.cs3.pl.parser.PrologCompiler;



public class CtChecker extends DefaultPrologTermParserVisitor {
	private ProblemCollector problemCollector;
	public CtChecker(ProblemCollector problemCollector) {
		super();
		this.problemCollector = problemCollector;
	}
	public Object visit(ASTCompoundTerm node, Object data) {
		
		String label= node.getLabel();
		if("ct".equals(label)){
			SimpleNode[] args = node.getArguments();
			if(args.length!=3){
				Problem p = TermParserUtils.createProblem(node,"Expected arity for 'ct' is 3. Found arity : "+args.length,Problem.WARNING);
			}
			if(!(args[1] instanceof ASTParanthesisTerm)){
				Problem p = TermParserUtils.createProblem(args[1],PrologCompiler.MSG_COND_PAREN,Problem.WARNING);
				problemCollector.reportProblem(p);
			}
			if(!(args[2] instanceof ASTParanthesisTerm)){
				Problem p = TermParserUtils.createProblem(args[2],PrologCompiler.MSG_ACTION_PAREN,Problem.WARNING);
				problemCollector.reportProblem(p);
			}
		}		
		
		
		return super.visit(node, data);
	}
}

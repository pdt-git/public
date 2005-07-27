package org.cs3.pl.parser.internal.term;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cs3.pl.parser.Problem;
import org.cs3.pl.parser.ProblemCollector;
import org.cs3.pl.parser.PrologCompiler;

public class SingletonChecker extends DefaultPrologTermParserVisitor {

	private final ProblemCollector problemCollector;

	private final Map singletons = new HashMap();
	
	public SingletonChecker(ProblemCollector problemCollector) {
		super();
		this.problemCollector = problemCollector;
	}
	
	public Object visit(ASTMember node, Object data) {
		singletons.clear();
		Object d =super.visit(node, data);
		Set keys = singletons.keySet();
		for (Iterator iter = keys.iterator(); iter.hasNext();) {
			String key = (String) iter.next();
			ASTVariable variable = (ASTVariable) singletons.get(key);
			if(variable!=null){
				reportSingleton(variable);
			}
		}
		return d;
	}
	

	public Object visit(ASTVariable node, Object data) {
		String image = node.getImage();
		if(image.startsWith( "_")){
			return super.visit(node, data);
		}
		if(singletons.containsKey(image)){
			singletons.put(image,null);
		}
		else{
			singletons.put(image,node);
		}
		return super.visit(node, data);
	}
	

	private void reportSingleton(ASTVariable variable) {
		String msg = PrologCompiler.MSG_SINGLETON_VAR_PREFIX+variable.getImage()+PrologCompiler.MSG_SINGLETON_VAR_POSTFIX;
		Problem problem = TermParserUtils.createProblem(variable,msg,Problem.WARNING);
		if(problemCollector!=null){
			problemCollector.reportProblem(problem);
		}
	}
	

}

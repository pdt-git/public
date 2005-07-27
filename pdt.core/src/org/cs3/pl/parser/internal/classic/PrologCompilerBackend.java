/*
 * Created on 01.04.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.parser.internal.classic;

import java.util.ArrayList;
import java.util.List;


/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PrologCompilerBackend extends ClassicPrologCompiler {

	private List problems = new ArrayList();
	
	public String getMsg(int i){
		return (String)problems.get(i * 2);
	}
	
	public Token getToken (int i){
		return (Token)problems.get(i * 2 + 1);
	}

	/**
	 * @param file
	 */
	public PrologCompilerBackend() {
		super();
	}
	/**
	 * @param node
	 * @throws CoreException
	 */
	protected void addProblem(Token token,String msg, int severity) {
		problems.add(msg);
		problems.add(token);
	}
	protected void resetProblems() {
		problems = new ArrayList();
	}
	
	public int getProblemNumber() {
		return problems.size() / 2;
	}
	
//	protected int getLineOffset(int line) {
//		return 0;
//	}

	}

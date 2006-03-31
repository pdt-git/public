/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pl.parser;

import java.util.List;
import java.util.Set;
import java.util.Vector;

import junit.framework.TestCase;

public class SyntaxTest extends TestCase implements ProblemCollector {
	private List problems = new Vector();
	public void test_PDT_34() {
		PrologCompiler compiler = getPrologCompiler();
		compiler.compile(":- module(knarz,[]),\ngut(ich).");
		Set set = compiler.getPublicModulePredicates();
		assertNotNull(set);
		assertEquals(getMessage(0),0,set.size());
	}
	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	public void testCT() {
		String clause = "ct(name,(classDefT(A,B,C,D),new_id(Class)),(add(classDefT(Class,B1,'NewName',[])))).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
	}

	/**
	 * 
	 * wrong number of parenthesis
	 */
	
	public void testCTFail() {
		String clause = "ct(name,(classDefT(A,B,C,D),new_id(Class),(add(classDefT(Class,B1,'NewName',[])))).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getProblemNumber() == 1);
	}
	
	public void testTokenMgr() {
		String clause = "A\\=A";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
	}
	
	public void testDisjunctions() {
		String clause = "new_idT(_H) :-\n(var(_H), new_id(_H));true.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);	
	}

	public void testNoArg() {
		String clause = "noarg :-\ntrue.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);	
	}

	public void testNumberFollowedByDot() {
		String clause = "ifNegAdd(_val, _val, 0, _) :- _val >= 0.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);	
	}
	
	public void testPredicateSignature() {
		String clause = ":- dynamic test_unify/1.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		
		assertTrue(getMessage(0),getProblemNumber() == 0);
		
		clause = ":- dynamic test_unify/0.";
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);

		clause = "test(dynamic).";
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);
	}
	
	
	
	public void testSmallerThan() {
		String clause = "compare(<,Element,Element).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);
	}
	
	public void testParenthesis() {
		String clause = "comma_prepend_(_T, ','(_comma,(_comma)), _T).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);
	}
	
	public void testMinusPlus() {
		String clause = "num(-1).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);
	}

	public void testSurroundingParenthesis() {
		String clause = "ct(name,classDefT(_,Parent,Name,[]),add(classDefT(1,B,'Parent',[]))).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		String msgs = "";
		
		for(int i=0;i<getProblemNumber();i++){
			msgs+=getProblem(i).message+";;";
		}
		assertTrue(msgs,msgs.indexOf(PrologCompiler.MSG_COND_PAREN)>=0);		
		assertTrue(msgs,msgs.indexOf(PrologCompiler.MSG_ACTION_PAREN)>=0);
	}

	public void testParameterizedCTName() {
		String clause = "ct(acget(Name),(classDefT(_,Parent,Name,[])),(add(classDefT(1,Parent,Name,[])))).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);
	}
	
	public void testIsSingleton() {
		String clause = "ct(acget(ClassName), " +
				"(classDefT(ID,Parent,ClassName,List),\n" +
				"Parent\\=ID)," +
				"(delete(classDefT(ID,Parent,ClassName,[])))).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getProblemNumber() == 1);
		assertEquals(PrologCompiler.MSG_SINGLETON_VAR_PREFIX + "List"+PrologCompiler.MSG_SINGLETON_VAR_POSTFIX,getProblem(0).message);
	}

	public void testNotSingleton() {
		String clause = "ct(acget(ClassName), (classDefT(ID,Parent,ClassName,[]),\nParent\\=ID),(delete(classDefT(ID,Parent,ClassName,[])))).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertTrue(getMessage(0),getProblemNumber() == 0);
	}

	public void testTokenOffsetTab() {
		String clause = "				test(A).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(1,getProblemNumber());
		assertEquals(10,getProblem(0).firstColumn);
	}

	

	public void testDollarInFunctor() {
		String clause = "$test(a).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}
	
	public void testSeveralDynamic() {
		String clause = ":- dynamic (^)/2, (ct)/4, (@)/2, ct/1, ct/2,ct/3.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());	
	}

	public void testDefineModule() {
		String clause = "user:goal_expansion( true).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}

	public void testVariableModule() {
		String clause = "p(A) :- A:aha(A).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
		clause = "p(a) :- aha.";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}
	
		public void testSpecialAtoms() {
		String clause = "p(a) :- p(:,$,:-,*->,=@=).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());	
	}
	
	public void testNotProvable() {
		String clause = "p(a) :- p(c), \\+ \\+ p(b).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}
	
	public void testMessage() { // TODO: what is this?
		String clause = "prolog:message(assumption_failed(G)) :- \n[ 'Assumption failed: ~p'-[G] ].";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
		clause = "prolog:message --> \n[ 'Assumption failed: ~p'-[a] ].";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}

	public void testVariableDividedAtom() { 
		String clause = "test(\\+ a).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}	

	public void testDoubleChar() { 
		String clause = "test(\"\"\"\",'''a''''').";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());	
	}

	public void testModuleExport() { 
		String clause = ":- module(gensym, [ reset_gensym/0, reset_gensym/1, gensym/2]).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());	
		assertEquals("gensym",checker.getModuleName());
	}

	
	public void testEmptyModuleExport() { 
		String clause = ":- module(gensym, []).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());			
	}
	
	public void testInitialization() { 
		String clause = ":- initialization p2(a).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}

	public void testBraces() { 
		String clause = "test :- {a,b}.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}

	public void testParenAroundClause() { 
		String clause = "user:(c(test) :- a,b).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}
	
	public void testCharAtomFunctor() { 
		String clause = "'$->'(a).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
		clause = "user:'$->'(a).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
	}
	public void testBinOctHex() { 
		String clause = "test(0x01,0o07,0b10,0xAFaf).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);	
		clause = "test(0b2)";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(1,getProblemNumber());	
	}
	
	public void testBitWise() { 
		String clause = "test(A,B) :- A xor B, A /\\ B, \\ B, A \\/ B.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());	
	}
	
	public void testCorrectAndUncorrectClauses() { 
		String clause = "test(C) :- C. test(A) :- A,. test(B) :- B.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(1,getProblemNumber());	
		
		assertEquals(1,checker.getClauses().size());
	}
	
	public void testSeveralErrors() { 
		String clause = "test(A) :- A,. test(B) :- B,.test(C) :- C,.test(D) :- D,. test(E) :- E.test(F) :- F.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(2,getProblemNumber());	
		assertEquals(2,checker.getClauses().size());
	}
	public void testWeirdStuff() { 
		String clause = "test(\\+ a).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());	
		clause = "test(A:A/A).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());	
		clause = "test(A:A).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());	
		clause = "test(a:a).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		clause = "style_chck(+dollar).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		clause = "test(0'$).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		clause = "test(A):- A^(a),t^(b).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		clause = "append(-, _).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		clause = "@(a). ^(b).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		
	}

	
	public void testWrongArity() { 
		String clause = ":- getFieldT(_Get,_parent, _, _Encl, _Recv, _Name, _VID),applyT(_call, _parent, _enclMethod, _origReceiver, _).";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(2,getProblemNumber());
		assertEquals("Expected arity for 'getFieldT' is 6, arity found: 7.", getProblem(0).message);
		assertEquals("Expected arity for 'applyT' is 7, arity found: 5.", getProblem(1).message);
	}
	


	

	public void testIntegerDivision() {
		String clause = "hello(X, Y) :- Y is 100 // X.\n";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
	}
	
	public void testFactbase() {
		String clause = "";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getProblemNumber(),0);
	}
	

	public void testAtomVsOperator(){
		String clause = "---.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		clause = "--.";
		 checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),1,getProblemNumber());
		clause = "----.";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),1,getProblemNumber());
		clause = "-----.";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
	}
	
	public void testModuleQualifiedSignatures(){
		String clause = ":- multifile a:b/2.";
		PrologCompiler checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		clause = ":- multifile a:b/2,c:d/2,e/2.";
		 checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),0,getProblemNumber());
		clause = ":- module(test,[a:b/2]).";
		checker = getPrologCompiler();
		checker.compile(clause);
		assertEquals(getMessage(0),1,getProblemNumber());		
	}
	private PrologCompiler getPrologCompiler() {
		PrologCompiler c = PrologCompilerFactory.create();
		c.setProblemCollector(this);
		return c;
	}

	public void reportProblem(Problem p) {
		problems.add(p);
		
	}

	public void reset() {
		problems.clear();
		
	}

	public void done() {
		;
	}

	private int getProblemNumber() {
		return problems.size();
	}

	private Problem getProblem(int i) {
		return (Problem) problems.get(i);
	}
	private String getMessage(int i){
		if(problems.size()>i){
			return getProblem(i).message;
		}
		return null;
	}
}

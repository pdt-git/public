/*
 * Created on 30.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl;


import java.util.Hashtable;

import jpl.Atom;
import jpl.Compound;
import jpl.PrologException;
import jpl.Query;
import jpl.Term;
import jpl.Variable;
import junit.framework.TestCase;

/**
 * @author windeln
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JPLTest extends TestCase {


	public JPLTest(String str) {
		super(str);
	}
	
	
	public JPLTest() {
		super("Prolog Manager Tests");
		jpl.JPL.init();
	}
	
	public void setUp() {
	}

	public void tearDown() {
		//manager.halt();
	}

	Query query;
	
	public Boolean call() {
//			Term[] args = new Term[0];
		if (query != null)
			query.rewind();
		//query = new jpl.Query("catch("+text+",CatchedException, true)");
		//query = new jpl.Query(text);
		Compound goal =new Compound("test",new Term[]{new Variable("A")});
		//query = new jpl.Query("test",new Term[]{new Variable("A")});
		query = new jpl.Query("catch",new Term[] {goal, new Variable("Exception"), new Atom("true")});
		try{
			boolean ret= query.hasSolution();
		
			ret = query.hasMoreSolutions();
			Hashtable solution = query.getSolution();
			Object exception =solution.get("CatchedException");
			return new Boolean(ret);
		} catch (Exception ex) {
			Debug.report(ex);
		}
		
		return null;
		
	}
	
	public Hashtable query(String text) {
		query = new jpl.Query(text);
		return next();
	}

	public Hashtable next() {
		if (query == null)
			return null;
//			Term[] args = new Term[0];
		//Query query =new Query("test2", new Term []{new Variable("_a")});
		if(query.hasMoreSolutions()){
			Hashtable solution = query.nextSolution();
			if (solution == null) return null;
			return solution;
			
		}
		query.close();
		query = null;
		return null;
	}

	public void testExceptionWrongSyntax() {
		try {
			Hashtable table = query("asdf(");
			fail("should have thrown exception");
		} catch(PrologException ex) {
		}
}
	
	public void testExceptionPredicateNotExists() {
		try {
			Hashtable table = query("asdf");
			fail("should have thrown exception");
		} catch(PrologException ex) {
		}
}

	
	public void testFormat() {
			Hashtable table = query("sformat(S,'~a,~a',[asdf,asd])");
			System.out.println(table.toString());
	}

	
	public void test3(){
		jpl.Query q = new jpl.Query("user:atom_concat(a,b,C)");
		if(q.hasMoreSolutions()){
			Hashtable solution = q.nextSolution();
			System.out.println("concat: " +solution);
		}

	}
	
	public void test3_flush(){
		jpl.Query q = new jpl.Query("user:atom_concat(a,b,C),flush_output");
		if(q.hasMoreSolutions()){
			Hashtable solution = q.nextSolution();
			System.out.println("concat: " +solution);
		}

	}
	public void test1() {
		assertTrue(query("assert(ntest(unv1))").size() == 0);
		assertTrue(query("assert(ntest(unv2))").size() == 0);
		assertTrue(query("assert(ntest(unv3))").size() == 0);
		assertTrue(query("assert(ntest(unv4))").size() == 0);
		assertTrue(query("assert(ntest(unv5))").size() == 0);
		Object result =query("ntest(A)").get("A");
		assertEquals(result.toString(),"unv1");
		while((result = next()) != null)
			System.out.println(""+result);
	}

}

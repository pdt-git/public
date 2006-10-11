/*
 * Created on Jun 24, 2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package alice.tuprolog.test;

import alice.tuprolog.*;
/**
 * @author aricci
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class TestParser {

	public static void main(String[] args) throws Exception {
		
		//Parser p = new Parser("n(+100).\n");
		//p.readTerm(true);
		//System.out.println(p.getCurrentTerm());		
	
		/*	
		Parser pa = new Parser("[p|Y]");
	    pa.readTerm(false);
		

		Struct p1 = new Struct(new Struct("p"),new Var("Y"));
		p1.resolveVariables();
		
		Struct p  = (Struct)pa.getCurrentTerm();
			
		System.out.println(p.equals(p1));
		*/
		
		String s = "{a,b,[3,{4,c},5],{a,b}}";
		//Struct result = new Struct("{}");
		Parser parser = new Parser(s);
		parser.readTerm(false);
		Term snew = parser.getCurrentTerm(); // OK
		
		//System.out.println(result);
		System.out.println(snew);
		

	}
}

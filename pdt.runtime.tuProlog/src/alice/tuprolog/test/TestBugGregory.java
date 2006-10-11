/*
 * Created on May 23, 2005
 * 
 */
package alice.tuprolog.test;

import alice.tuprolog.*;

/**
 * 
 *
 * @author aricci
 *
 */
public class TestBugGregory {

    public static void main(String[] args) throws Exception {
        	Theory theory = new Theory("a :- b, (d ; e).\ne.");
        	Term query = new Struct("a");

        	Prolog engine = new Prolog();

        	engine.setTheory(theory);
        	Theory before = engine.getTheory();
        	System.out.println("Before:\n" + before);

        	System.out.println("?- " + query + ".");
        	System.out.println(engine.solve(query).isSuccess());

        	engine.setTheory(before);
        	Theory after = engine.getTheory();
        	System.out.println("\nAfter:\n" + after);

        	System.out.println("?- " + query + ".");
        	System.out.println(engine.solve(query).isSuccess());

    }
}

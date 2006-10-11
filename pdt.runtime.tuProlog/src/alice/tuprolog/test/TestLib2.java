/*
 * Created on Jan 11, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package alice.tuprolog.test;

import alice.tuprolog.*;
/**
 * @author aricci
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TestLib2 {

	public static void main(String[] args) throws Exception {

		Prolog engine = new Prolog();
		
        //engine.loadLibrary("alice.tuprolog.test.TestLibrary");
		
		
		
		Library lib = new TestLibrary();
		engine.loadLibrary(lib);

		engine.solve("N is sum(1,3), println(sum(N)).");
		
	}
}

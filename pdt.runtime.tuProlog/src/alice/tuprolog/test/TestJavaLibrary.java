package alice.tuprolog.test;

import alice.tuprolog.*;
import alice.tuprolog.lib.*;

/**
 * Miscellaneous tests on the JavaLibrary
 * 
 */
public class TestJavaLibrary {
	
	private int id = 0;	
	
	public void check(){
		System.out.println("CHECK "+id);
	}
	
	public void update(){
		id++;
	}
	
	
	public static void main(String[] args) throws Exception {
	
		Prolog engine = new Prolog();
		engine.setSpy(true);
		
		JavaLibrary lib = (JavaLibrary) engine.getLibrary("alice.tuprolog.lib.JavaLibrary");
		
		// Testing anonymous registration of objects
		
		String theory =
			"demo(X):-			\n"+
			"	X <- update.		\n";
			
		engine.setTheory(new Theory(theory));
		TestJavaLibrary obj = new TestJavaLibrary();		
		obj.check();
		Struct t = lib.register(obj);
		engine.solve(new Struct("demo",t));
		obj.check();
		// check unregistering
		
		lib.unregister(t);
		SolveInfo sol = engine.solve(new Struct("demo",t));
		System.out.println(sol);
		
		// Testing dynamic retrieval of objects created during Prolog computation
		
		theory =
			"demo(Y):-													\n"+
			"	java_object('alice.test.tuprolog.TestJavaLibrary',[],Y),	\n"+
			"	Y <- update, 											\n"+
			"	Y <- update.												\n";
			
		engine.setTheory(new Theory(theory));
		SolveInfo info = engine.solve("demo(Obj).");
		Struct id = (Struct)info.getVarValue("Obj");
		TestJavaLibrary obj2 = (TestJavaLibrary)lib.getRegisteredDynamicObject(id);
		obj2.check();
		
	}
}

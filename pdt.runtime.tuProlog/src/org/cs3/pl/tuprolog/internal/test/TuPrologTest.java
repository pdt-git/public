package org.cs3.pl.tuprolog.internal.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Date;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Locale;

import junit.framework.TestCase;

import org.cs3.pl.tuprolog.internal.TuProlog;

import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoMoreSolutionException;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Theory;
import alice.tuprolog.event.SpyEvent;
import alice.tuprolog.event.SpyListener;

public class TuPrologTest extends TestCase {

	private static final String TEMPDIR = "java.io.tmpdir";
	TuProlog engine; 
	protected void setUp() throws Exception {
    	engine = new TuProlog();
   // 	engine.initEngine();
		//TuProlog tuProlog = new TuProlog();
    	engine.addSpyListener(new SpyListener() {
			public void onSpy(SpyEvent e) {
				File file = new File("c:/temp/tuProlog.log");
				try {
					FileWriter write = new FileWriter(file,true);
					write.write(e.getMsg() + "\n");
					write.close();
				} catch (IOException e1) {
				}
			}
    	});
	}

	public void testWriteToFile() throws Exception {
		String compare = "written by tuProlog";
		writeAndCompare("print('"+compare+"')", compare);
		writeAndCompare("A = 123, print(A), print('B')", "123B");
	}

	private void writeAndCompare(String write, String compare) throws MalformedGoalException, FileNotFoundException, IOException {
		String path = System.getProperty(TEMPDIR).replace(File.separatorChar, '/')+"$tuPrologFileOutputTestFile$.pl";
		engine.solve("tell('"+path+"'), "+write + ", told.");
		BufferedReader reader = new BufferedReader(new FileReader(path));
		assertEquals(compare,reader.readLine());
		reader.close();
	}
	
	public void testPlus() throws Exception {
    	predicateIsTrue("plus(1,2,3).");
    	predicateIsTrue("plus(A,2,3), A = 1.");
    	predicateIsTrue("plus(1,2,C), C=3.");
    	predicateIsTrue("plus(1,B,3), B=2.");
    	predicateFails("plus(A,B,3).");

	}

	private void predicateIsTrue(String query) throws MalformedGoalException {
		assertTrue(engine.solve(query).isSuccess());
	}
	
	private void predicateFails(String query) throws MalformedGoalException {
		assertFalse(engine.solve(query).isSuccess());
	}
	
	public void testIsDynamic() throws Exception {
//		Theory theory = new Theory(	":- dynamic is_dynamic/1.\n");
//		engine.addTheory(theory);
		
    	SolveInfo result = engine.solve("is_dynamic(is_dynamic/1).");
    	assertTrue(result.isSuccess() );
    	
    	result = engine.solve("is_dynamic(is_multifile/1).");
    	assertTrue(result.isSuccess() );

	}
	public void testStruct() throws Exception {

//    	engine.solve(new Struct())
	}
		
	public void testCall() throws Exception {
			Theory theory = new Theory(":- assert(testpredicate(arg1)).");
	    	engine.addTheory(theory);
//			engine.solve("assert(testpredicate(a)).");
	    	assertEquals("arg1",engine.solve("testpredicate(Arg).").getTerm("Arg").toString());
//	    	printClauses();
	    	
	}
	
	public void testNonDefinedPredicate() throws Exception {
		Theory theory = new Theory("factbase(arg1).");
    	engine.addTheory(theory);
//		engine.solve("assert(testpredicate(a)).");
    	SolveInfo info = engine.solve("factbase1(Arg).");
//    	printClauses();
    	
    }
//	public void testCall() throws Exception{
//	Theory theory = new Theory("a(a).\n :- a(a).");
//	engine.addTheory(theory);
//}
	
	
	protected void tearDown() throws Exception {
		engine.clearTheory();
	}

	public void testClause() throws Exception{
		Theory theory = new Theory("a(a).\n a(b).\n :- asserta(a(c)).\n");

    	engine.addTheory(theory);

    	SolveInfo result = engine.solve("clause(a(A),B).");

    	assertEquals("c", result.getTerm("A").toString());
    	assertEquals("a", engine.solveNext().getTerm("A").toString());
    	assertEquals("b", engine.solveNext().getTerm("A").toString());
	}

	
	public void testJTLinkFile() throws Exception{
		
		engine.loadLibrary( "javaFactFileLinking.pl");
		engine.loadLibrary( "reduced_jt_engine.pl");
    	engine.addTheory(new Theory(
    			getClass().getResourceAsStream("testlink.pl")));
    	
    	engine.addSpyListener(new SpyListener() {
			public void onSpy(SpyEvent e) {
				System.out.println(e.toString());
			}
    	});
    	
    	
    	assertTrue(engine.solve("inTe(aspect(lId(4))).").isSuccess());
    		
//        	SolveInfo result = engine.solve("aspect(AspectID).");
//assertEquals("asdf", result.getTerm("AspectID").toString());
    	
	}

	private void printClauses() throws MalformedGoalException, NoMoreSolutionException {
		SolveInfo result = engine.solve("clause(A,B).");
    	while(result.isSuccess()) {
    		System.out.println(result.toString());
    		result = engine.solveNext();
    	}
	}	
	
	public void testInvalidCall() throws Exception{
    	try {
			Theory theory = new Theory("a(a).\n :- notx existing(A).");
			engine.setWarning(true);
	    	engine.addTheory(theory);
			fail("expected InvalidTheoryException");
		} catch (InvalidTheoryException e) {
			assertEquals(2, e.line);
			assertEquals(24, e.pos);
		}
	}	
	
	public void testFindAll() throws Exception {
		Theory theoryFacts = new Theory("a(a).\na(b).\n");
		Theory theoryClause = new Theory("findall_pred(L) :-\n   findall(A,a(A),L).\n");
   	
    	engine.addTheory(theoryFacts);
    	engine.addTheory(theoryClause);
    	SolveInfo result = engine.solve("findall_pred(L).");
    	assertEquals("[a,b]", result.getVarValue("L").toString());
	}
	
	public void testInvalidTheory() throws Exception{
    	try {
			Theory theory = new Theory("a(a).\n a(b).\nfindall_pred(L) :- findall(A,a(A),L.\n");
	    	engine.addTheory(theory);
	    	
			fail("expected InvalidTheoryException");
		} catch (InvalidTheoryException e) {
			assertEquals(4, e.line);
			assertEquals(50, e.pos);
		}
	}
	
	

	
	public void testInvalidDynamic() throws Exception{
		Theory theory = new Theory(
				":- op(1150, fx, dynamic). \n" +
				":- op(1150, fx, multifile). \n" +
				":- dynamic test/2. \n" +
				":- multifile test/2."
				);
    	engine.addTheory(theory);
	}

	public void testTemp() throws Exception {
		Calendar calendar = Calendar.getInstance(Locale.GERMANY);
		System.out.println(new SimpleDateFormat("E MMM dd HH:mm:ss z yyyy", Locale.UK).format(
			new Date(1000)));
	}
	
	public void testSync()throws Exception {
		
		engine.addSpyListener(new SpyListener() {

			public void onSpy(SpyEvent e) {
				System.err.println(e.getMsg());
				
			}
			
		});
		
		SolveInfo info = engine.solve("A=@=B.");
		assertTrue(info.isSuccess());
		engine.loadLibrary("sync.pl");
		engine.loadLibrary("sync_test.pl");
		info = engine.solve("setUp(term_ref).");
		assertTrue(info.isSuccess());
		engine.setSpy(true);
		engine.loadLibrary("localisation.pl");
		
		info = engine.solve("test(init_idb).");
		assertTrue(info.isSuccess());
		
		info = engine.solve("test(separate_functor_arity_module_safe).");
		assertTrue(info.isSuccess());

		
	}
}

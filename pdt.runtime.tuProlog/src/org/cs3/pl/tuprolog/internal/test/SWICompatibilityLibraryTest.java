package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;

import org.cs3.pl.tuprolog.internal.SWICompatibilityLibrary;

import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.event.OutputEvent;
import alice.tuprolog.event.OutputListener;

public class SWICompatibilityLibraryTest extends TestCase {
	private Prolog engine;
	
	protected void setUp() throws Exception {
		super.setUp();
		if ( engine==null){
			engine = new Prolog();
			engine.loadLibrary(new SWICompatibilityLibrary());
		}
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	
	private void structuralEqualityAtoms() throws	MalformedGoalException,
											NoSolutionException {
		SolveInfo info = engine.solve("a=@=b.");
		assertFalse("Structural Equality between Atoms should be false.", info.isSuccess());
	}

	private void structuralEqualityVars() throws	MalformedGoalException,
											NoSolutionException {

		SolveInfo info = engine.solve("_=@=_.");
		assertTrue("Structural Equality between Atoms should be true.", info.isSuccess());
	}
	
	private void structuralEqualityCompoud() throws	MalformedGoalException,
												NoSolutionException {

		SolveInfo info = engine.solve("x(B,3,a,B)=@=x(A,3,b,A).");
		assertFalse("Structural Equality between Compound should be false.", info.isSuccess());
	}

	private void structuralEqualityCompoud0() throws	MalformedGoalException,
												NoSolutionException {
		
		SolveInfo info = engine.solve("x(B,3,a,B)\\=@=x(A,3,b,A).");
		assertTrue("Structural Equality between Compound should be true.", info.isSuccess());
	}
	
	private void structuralEqualityCompoud1() throws	MalformedGoalException,
												NoSolutionException {
		
		SolveInfo info = engine.solve("x(B,C,B,B)=@=x(A,C,A,A).");
		assertTrue("Structural Equality between Compound should be true.", info.isSuccess());
	}

	private void structuralEqualityCompoud2() throws	MalformedGoalException,
												NoSolutionException {
		
		SolveInfo info = engine.solve("x(A,A,C,D)=@=x(A,C,D,D).");
		assertFalse("Structural Equality between Compound should be false.", info.isSuccess());
		
		//FIXME SWI-Prolog answers no for the following queries.
		info = engine.solve("x(A,A,C,D)=@=x(D,D,C,B).");
		assertTrue("Structural Equality between Compound should be true.", info.isSuccess());
		
		info = engine.solve("x(A,A,C,D)=@=x(D,D,C,D).");
		assertFalse("Structural Equality between Compound should be false.", info.isSuccess());
	}
	
	private void structuralEqualityCompoud3() throws	MalformedGoalException,
												NoSolutionException {
		SolveInfo info = engine.solve("y(x(A,A),B)=@=y(x(B,B),A).");
		assertTrue("Structural Equality between Compound should be true.", info.isSuccess());
	}
	
	private void structuralEqualityList() throws	MalformedGoalException,
												NoSolutionException {

		SolveInfo info = engine.solve("[A,C,B,B]=@=[A,C,B,A].");
		assertFalse("Structural Equality between List should be false.", info.isSuccess());
	}

	private void structuralEqualityList2() throws	MalformedGoalException,
											NoSolutionException {

		SolveInfo info = engine.solve("[A,C,B,B]=@=[B,C,A,A].");
		assertTrue("Structural Equality between List should be true.", info.isSuccess());
	}
	

	public void testStructuralEquality() throws Exception{
	
		structuralEqualityAtoms();
		structuralEqualityVars();
		structuralEqualityCompoud();
		structuralEqualityCompoud0();
		structuralEqualityCompoud1();
		structuralEqualityCompoud2();
		structuralEqualityCompoud3();
		structuralEqualityList();
		structuralEqualityList2();		
	}
	
	
	public void testModuleOperator() throws MalformedGoalException,
											NoSolutionException {
		SolveInfo info = engine.solve("testing_module:assert(first(x)).");
		assertTrue("Calling Module failed.", info.isSuccess());
		info = engine.solve("first(x).");
		assertTrue("Calling Module Failed", info.isSuccess());
	}
	
	public void testThrow() throws Exception {
		
			SolveInfo inf = engine.solve("assert(example(1)).");
			SolveInfo info = engine.solve(" example(A), catch( " +
										  "throw('testing')," +
										  "'testing'," +
										  "recorda('throw_test',testing)" +
										  "), format('test: ', A).");
			
//			System.err.println(info.getBindingVars());
			assertTrue("Failed to throw an exception", info.isSuccess());
			
			info = engine.solve("recorded('throw_test',X).");
			assertTrue("Failed to find a record with throw_test", info.isSuccess());
			assertEquals("testing",info.getVarValue("X").toString());
		
	}
	
	public void testThrowBindings() throws Exception{

		SolveInfo info = engine.solve("assert(example(2)).");
		info = engine.solve("catch( " +
				"throw(example(2))," +
				"example(X)," +
				"format('example = ~w~n',X)).");

		assertTrue(info.getTerm("X").toString().equals("2"));		
	}
	
	
	
	private void recorda_2() throws Exception {

		SolveInfo info = engine.solve("recorda(key_1, value_1).");
		assertTrue(info.isSuccess());
	}

	private void recorda_3() throws Exception {
		
		SolveInfo info = engine.solve("recorda(key_2, value_2, Ref_1).");
		assertTrue(info.isSuccess());
//		System.err.println("Ref :" + info.getVarValue("Ref_1"));
	}

	private void recordz_2() throws Exception {
		
		SolveInfo info = engine.solve("recordz(key_3, value_3).");
		assertTrue(info.isSuccess());		
	}

	private void recordz_3() throws Exception {

		SolveInfo info = engine.solve("recordz(key_4 ,value_4, Ref_2).");
		assertTrue(info.isSuccess());
//		System.err.println("Ref :" + info.getVarValue("Ref_2"));
	}

	private void recorded_2() throws Exception {
		
		SolveInfo info = engine.solve("recordz(key_5, value_5).");
		assertTrue(info.isSuccess());
		
		info = engine.solve("recorded(key_5, value_5).");
		assertTrue(info.isSuccess());
	}

	private void recorded_3() throws Exception {
		
		SolveInfo info = engine.solve("recordz(key_6, value_6(a)).");
		assertTrue(info.isSuccess());
		
		info = engine.solve("recordz(key_6, value_6(b)).");
		assertTrue(info.isSuccess());
		
		info = engine.solve("recorded(key_6, Values, Ref_3).");
		assertTrue(info.isSuccess());	
		
		
		//FIXME recorded should return all the records, now it only returns the first one.
		if ( info.hasOpenAlternatives()) {
			info = engine.solveNext();
		//	System.err.println(info.getBindingVars());
		}
		
		//assertEquals(info.getVarValue("Ref"), info2.getVarValue("Ref"));
		//System.err.println("Ref:"+ info.getVarValue("Ref"));
	}
	
	private void erase_1() throws Exception {
		
		SolveInfo info = engine.solve("recordz(key_7, value_7, Ref_7).");
		assertTrue(info.isSuccess());
		
		SolveInfo info2 = engine.solve("recorded(key_7, value_7, Ref_7), erase(Ref_7).");
		assertTrue(info2.isSuccess());	
		
		info2 = engine.solve("recorded(key_7, value_7, Ref_7).");
		assertFalse(info2.isSuccess());	
	}
	
	
	public void testRecorded() throws Exception{
		engine.addOutputListener( new OutputListener(){

			public void onOutput(OutputEvent e) {
//				System.err.println(e.getMsg());				
			}
			
		});
		
		recorda_2();
		recorda_3();
		recordz_2();
		recordz_3();
		recorded_2();
		recorded_3();
		erase_1();
	}
	
	public void testWithMutex() throws Exception{
		SolveInfo info = engine.solve("with_mutex('mutex_test',recordz(hasan,ops, Ref)).");
		assertTrue(info.isSuccess());

	}
}

package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;

import org.cs3.pl.tuprolog.internal.SWICompatibilityLibrary;

import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Theory;

public class SWICompatibilityLibraryTest extends TestCase {
	private Prolog engine;
	
	protected void setUp() throws Exception {
		super.setUp();
		if ( engine==null){
			engine = new Prolog();
			Theory record_theory = new Theory("ref(1). 		  \n" +
					"recorda(Key, Value, Ref):- 				" +
					"	var(Ref), ref(CrRef), Ref is CrRef+1,	" +
					" 	asserta(record_db(Key, Value, Ref)),	" +
					"	update_ref. 						  \n" +
					"recordz(Key, Value, Ref):- 				" +
					"	var(Ref), ref(CrRef), Ref is CrRef+1,	" +
					"	assertz(record_db(Key, Value, Ref)),	" +
					"	update_ref. 						  \n" +
					"recorded(Key, Value, Ref):-				" +
					"	var(Ref), record_db(Key, Value, Ref). \n" +
					"erase(Ref):- 								" +
					"	nonvar(Ref), 							" +
					"	retract(record_db( _, _, Ref)). 	  \n" +
					"recordz(Key, Value):- 						" +
					"	recordz(Key, Value, _). 			  \n" +
					"recorda(Key, Value):- 						" +
					"	recorda(Key, Value, _). 			  \n" +
					"recorded(Key, Value):- 					" +
					"	recorded(Key, Value, _). 			  \n" +
					"update_ref:-								" +
					"	ref(CrRef), retract(ref(_)),			" +
					"	NxtRef is CrRef+1, assert(ref(NxtRef)).	");
			engine.addTheory(record_theory);
			//engine.loadLibrary(new SWICompatibilityLibrary());
		}
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	public void testStructeqAtoms() throws	MalformedGoalException,
											NoSolutionException {
		SolveInfo info = engine.solve("a=@=b.");
		assertFalse("Failed to Query engine", info.isSuccess());
	}

	public void testStructeqVars() throws	MalformedGoalException,
											NoSolutionException {

		SolveInfo info = engine.solve("_=@=_.");
		assertTrue("Failed to Query engine", info.isSuccess());
	}
	
	public void testStructeqCompoud() throws	MalformedGoalException,
												NoSolutionException {

		SolveInfo info = engine.solve("x(B,3,a,B)=@=x(A,3,b,A).");
		assertFalse("Failed to Query engine", info.isSuccess());
	}

	public void testStructeqCompoud0() throws	MalformedGoalException,
												NoSolutionException {
		
		SolveInfo info = engine.solve("x(B,3,a,B)\\=@=x(A,3,b,A).");
		assertTrue("Failed to Query engine", info.isSuccess());
	}
	
	public void testStructeqCompoud1() throws	MalformedGoalException,
												NoSolutionException {
		
		SolveInfo info = engine.solve("x(B,C,B,B)=@=x(A,C,A,A).");
		assertTrue("Failed to Query engine", info.isSuccess());
	}

	public void testStructeqCompoud2() throws	MalformedGoalException,
												NoSolutionException {

		SolveInfo info = engine.solve("x(A,A,C,D)=@=x(A,C,D,D).");
		assertFalse("Failed to Query engine", info.isSuccess());
		info = engine.solve("x(A,A,C,D)=@=x(D,D,C,B).");
		assertTrue("Failed to Query engine", info.isSuccess());
	}
	
	public void testStructeqCompoud3() throws	MalformedGoalException,
												NoSolutionException {
		SolveInfo info = engine.solve("y(x(A,A),B)=@=y(x(B,B),A).");
		assertTrue("Failed to Query engine", info.isSuccess());
	}
	public void testStructeqList() throws	MalformedGoalException,
												NoSolutionException {

		SolveInfo info = engine.solve("[A,C,B,B]=@=[A,C,B,A].");
		assertFalse("Failed to Query engine", info.isSuccess());
	}

	public void testStructeqList2() throws	MalformedGoalException,
											NoSolutionException {

		SolveInfo info = engine.solve("[A,C,B,B]=@=[B,C,A,A].");
		assertTrue("Failed to Query engine", info.isSuccess());
	}
	
	public void testModuleOperator() throws MalformedGoalException,
											NoSolutionException {
		SolveInfo info = engine.solve("geko:assert(test_assert(x)).");
		assertTrue("Failed to Query Engine ", info.isSuccess());
		info = engine.solve("test_assert(x).");
		assertTrue("Failed to Query Engine ", info.isSuccess());
	}
	
	public void testThrow() throws Exception {
		
			SolveInfo info = engine.solve(" catch( " +
										  "throw('my_prolog_exception')," +
										  "my_prolog_exception," +
										  "recorda('throw_test',testing)" +
										  ").");
			assertTrue("Failed to throw an exception", info.isSuccess());
			
			info = engine.solve("recorded('throw_test',X).");
			assertTrue("Failed to find a record with throw_test", info.isSuccess());
			assertEquals("testing",info.getVarValue("X").toString());
		
	}
	
	public void testRecorda_2() throws Exception {

		SolveInfo info = engine.solve("recorda(test_key, test_value).");
		assertTrue(info.isSuccess());
	}

	public void testRecorda_3() throws Exception {
		
		SolveInfo info = engine.solve("recorda(test_key,test_value, Ref).");
		assertTrue(info.isSuccess());
	}

	public void testRecordz_2() throws Exception {
		
		SolveInfo info = engine.solve("recordz(test_key, test_value).");
		assertTrue(info.isSuccess());		
	}

	public void testRecordz_3() throws Exception {

		SolveInfo info = engine.solve("recordz(test_key, test_value, Ref).");
		assertTrue(info.isSuccess());
	}

	public void testRecorded_2() throws Exception {
		
		SolveInfo info = engine.solve("recordz(test_key,test_value).");
		assertTrue(info.isSuccess());
		info = engine.solve("recorded(test_key, test_value).");
		assertTrue(info.isSuccess());
	}

	public void testRecorded_3() throws Exception {
		
		SolveInfo info = engine.solve("recordz(test_key, test_value(1)).");
		assertTrue(info.isSuccess());
		info = engine.solve("recordz(test_key, test_value(2)).");
		assertTrue(info.isSuccess());
		SolveInfo info2 = engine.solve("recorded(test_key, X, Ref).");
		assertTrue(info2.isSuccess());	
		assertEquals("test_value(1)", info2.getVarValue("X").toString());
		if ( info2.hasOpenAlternatives()) {
			info2 = engine.solveNext();
			assertEquals("test_value(2)", info2.getVarValue("X").toString());
		}
	
	}
	
	public void testErase_1() throws Exception {
		
		SolveInfo info = engine.solve("recordz(test_key,test_value(1), Ref).");
		assertTrue(info.isSuccess());
		
		SolveInfo info2 = engine.solve("recorded(test_key, test_value(1), Ref), erase(Ref).");
		assertTrue(info2.isSuccess());	
		
		info2 = engine.solve("recorded(test_key,test_value(1), Ref).");
		assertFalse(info2.isSuccess());	
	}
	
	public void testWithMutex() throws Exception{
		SolveInfo info = engine.solve("with_mutex('mutex_test',recordz(test_key, test_value, Ref)).");
		assertTrue(info.isSuccess());

	}
}

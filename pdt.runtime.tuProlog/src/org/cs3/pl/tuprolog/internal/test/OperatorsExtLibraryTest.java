package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;

import org.cs3.pl.tuprolog.internal.OperatorsExtLibrary;

import alice.tuprolog.InvalidLibraryException;
import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.Library;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.Parser;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Term;
import alice.tuprolog.Theory;
import alice.tuprolog.event.LibraryEvent;
import alice.tuprolog.event.LibraryListener;
import alice.tuprolog.event.QueryEvent;
import alice.tuprolog.event.QueryListener;
import alice.tuprolog.event.WarningEvent;
import alice.tuprolog.event.WarningListener;

public class OperatorsExtLibraryTest extends TestCase {
	private Prolog engine;
	
	protected void setUp() throws Exception {
		super.setUp();
		if ( engine==null){
			engine = new Prolog();
			engine.loadLibrary(new OperatorsExtLibrary());
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

		SolveInfo info = engine.solve("A=@=B.");
		assertTrue("Failed to Query engine", info.isSuccess());
	}
	
	public void testStructeqCompoud() throws	MalformedGoalException,
												NoSolutionException {

		SolveInfo info = engine.solve("x(B,3,a,B)=@=x(A,3,b,A).");
		assertFalse("Failed to Query engine", info.isSuccess());
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
}

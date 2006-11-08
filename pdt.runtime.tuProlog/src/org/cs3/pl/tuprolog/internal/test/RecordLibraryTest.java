package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;

import org.cs3.pl.tuprolog.internal.RecordLibrary;

import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;

public class RecordLibraryTest extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testRecorda_2() throws Exception {
		Prolog engine = new Prolog();
		engine.loadLibrary(new RecordLibrary());
		
		SolveInfo info = engine.solve("recorda(hasan,ops,Ref).");
		assertTrue(info.isSuccess());
		System.err.print(info.getBindingVars());
		//info = engine.solve("recorded(Y,X).");
		info = engine.solve("recorda(hasan,opss,Ref).");
		assertTrue(info.isSuccess());
		System.err.print(info.getBindingVars());
		
		info = engine.solve("recorded(hasan,ops,Ref).");
		assertTrue(info.isSuccess());
		System.err.print(info.getBindingVars());

	}

	public void testRecorda_3() {
		fail("Not yet implemented");
	}

	public void testRecordz_2() {
		fail("Not yet implemented");
	}

	public void testRecordz_3() {
		fail("Not yet implemented");
	}

	public void testRecorded_2() {
		fail("Not yet implemented");
	}

	public void testRecorded_3() {
		fail("Not yet implemented");
	}

}

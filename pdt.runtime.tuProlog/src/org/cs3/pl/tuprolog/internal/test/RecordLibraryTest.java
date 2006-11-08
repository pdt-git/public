package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;

import org.cs3.pl.tuprolog.internal.RecordLibrary;

import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;

public class RecordLibraryTest extends TestCase {
	private Prolog engine = null ;

	protected void setUp() throws Exception {
		super.setUp();
		if ( engine == null ){
			engine = new Prolog();
			engine.loadLibrary(new RecordLibrary());
		}
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testRecorda_2() throws Exception {

		SolveInfo info = engine.solve("recorda(hasan,ops).");
		assertTrue(info.isSuccess());
	}

	public void testRecorda_3() throws Exception {
		
		SolveInfo info = engine.solve("recorda(hasan,ops, Ref).");
		assertTrue(info.isSuccess());
		System.err.println("Ref :" + info.getVarValue("Ref"));
	}

	public void testRecordz_2() throws Exception {
		
		SolveInfo info = engine.solve("recordz(hasan,ops).");
		assertTrue(info.isSuccess());		
	}

	public void testRecordz_3() throws Exception {

		SolveInfo info = engine.solve("recordz(hasan,ops, Ref).");
		assertTrue(info.isSuccess());
		System.err.println("Ref :" + info.getVarValue("Ref"));
	}

	public void testRecorded_2() throws Exception {
		
		SolveInfo info = engine.solve("recordz(hasan,ops).");
		assertTrue(info.isSuccess());
		info = engine.solve("recorded(hasan,ops).");
		assertTrue(info.isSuccess());
	}

	public void testRecorded_3() throws Exception {
		
		SolveInfo info = engine.solve("recordz(hasan,ops, Ref).");
		assertTrue(info.isSuccess());
		SolveInfo info2 = engine.solve("recorded(hasan,ops, Ref).");
		assertTrue(info2.isSuccess());	
		
		assertEquals(info.getVarValue("Ref"), info2.getVarValue("Ref"));
		System.err.println("Ref:"+ info.getVarValue("Ref"));
	}
	
	public void testErase_1() throws Exception {
		
		SolveInfo info = engine.solve("recordz(hasan,ops, Ref).");
		assertTrue(info.isSuccess());
		
		SolveInfo info2 = engine.solve("recorded(hasan,ops, Ref), erase(Ref).");
		assertTrue(info2.isSuccess());	
		
		info2 = engine.solve("recorded(hasan,ops, Ref).");
		assertFalse(info2.isSuccess());	
	}

}

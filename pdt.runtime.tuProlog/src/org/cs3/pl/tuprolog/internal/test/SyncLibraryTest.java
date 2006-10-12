package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;

public class SyncLibraryTest extends TestCase {

	static protected Prolog engine=null;
	
	protected void setUp() throws Exception {
		if (engine==null){
			engine = new Prolog();
			engine.loadLibrary("org.cs3.pl.tuprolog.internal.SyncLibrary");
		}
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testSync_0() throws Exception {
		SolveInfo info = engine.solve("sync.");
		if (!info.isSuccess())
			fail("Failed to query sync().");
	}

	public void testWith_mutex_2() throws Exception {
		SolveInfo info = engine.solve("with_mutex(ne,sync).");
		if (!info.isSuccess())
			fail("Failed to query with_mutex().");
		
	}

}

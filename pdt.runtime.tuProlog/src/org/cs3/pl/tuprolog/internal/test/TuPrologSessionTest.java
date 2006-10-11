package org.cs3.pl.tuprolog.internal.test;

import org.cs3.pl.tuprolog.internal.TuPrologSession;

import junit.framework.TestCase;

public class TuPrologSessionTest extends TestCase {
	public void testNormalizeQuery() throws Exception {
		assertEquals("asdf.", TuPrologSession.normalizeQuery("asdf.   "));
		assertEquals("asdf.", TuPrologSession.normalizeQuery("asdf.\n   "));
		assertEquals("asdf.", TuPrologSession.normalizeQuery("asdf.\n"));
		assertEquals("asdf.", TuPrologSession.normalizeQuery("asdf  \n \n "));
	}
}

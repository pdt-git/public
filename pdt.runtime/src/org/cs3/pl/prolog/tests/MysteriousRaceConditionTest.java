package org.cs3.pl.prolog.tests;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;

public class MysteriousRaceConditionTest extends TestCase {

	private PrologInterface pif;

	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		
		pif = PrologInterfaceFactory.newInstance().create();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		pif.stop();
	}

	public void testMyteriousRaceCondition() throws PrologInterfaceException {
		
		PrologSession session = pif.getSession();
		
		String workspace="/home/lukas/workspace";
		String query = 
				"(	user:file_search_path(library, '"+workspace+"/pdt.runtime/library/attic')" +
				"	->	true" +
				"	;	user:assert(file_search_path(library, '"+workspace+"/pdt.runtime/library/attic')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.runtime/library/attic')))," +
				"(	user:file_search_path(library, '"+workspace+"/pdt.runtime/library/pif')" +
				"->	true" +
				";	user:assert(file_search_path(library, '"+workspace+"/pdt.runtime/library/pif')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.runtime/library/pif')))," +
				"(	user:file_search_path(library, '"+workspace+"/pdt.runtime/library/pdt')" +
				"->	true" +
				";	user:assert(file_search_path(library, '"+workspace+"/pdt.runtime/library/pdt')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.runtime/library/pdt')))," +
				"(	user:file_search_path(library, '"+workspace+"/pdt.runtime/library/common')" +
				"->	true" +
				";	user:assert(file_search_path(library, '"+workspace+"/pdt.runtime/library/common')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.runtime/library/common')))," +
				"(	user:file_search_path(library, '"+workspace+"/pdt.core/engine')" +
				"->	true" +
				";	user:assert(file_search_path(library, '"+workspace+"/pdt.core/engine')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.core/engine')))";
		session.queryOnce(query);
		session.queryOnce("ensure_loaded(library('facade/pdt_facade'))");
		session.queryOnce("guitracer");
		session.queryOnce("tspy(parse:spyme)"); //this predicate exists.
		PrologSession session2 = pif.getSession();
		

	}

}

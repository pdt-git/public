/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.internal.punit;

import java.util.Map;

import junit.framework.TestCase;

import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;

public class TestPredicateTest extends TestCase {
	 
	PrologInterface pif;
	private IPrologInterfaceAdapter adapter; 
	
	public TestPredicateTest() {
		super();
	}
	
	public TestPredicateTest(String testname,IPrologInterfaceAdapter adapter) {
		super(testname);
		this.adapter = adapter;
	}
	
	@Override
	protected void runTest() throws PrologException, PrologInterfaceException {
		String resultString = "";
		boolean failed = false;
		PrologSession session = null;
		try {
			pif = adapter.getPrologInterface();
			session = pif.getSession();

			if (session.queryOnce("clause(setUp('" + getName() + "'),_)") != null
					&& session.queryOnce("setUp('" + getName() + "')") == null)
				fail("Error executing predicate \"setUp/1\".\n");

			String kind = null;
			String comment = null;

			Map<String,Object> queryResult = session.queryOnce("junit_adapter('" + getName()
					+ "',Kind,Comment)");
			if (queryResult != null) {
				kind = (String) queryResult.get("Kind");
				comment = (String) queryResult.get("Comment");
			} else {
				resultString += "Error executing predicate \"junit_adapter/3\".\n";
				failed = true;
			}

			String testMessage = "";
			if (session.queryOnce("clause(tearDown('" + getName() + "'),_)") != null
					&& session.queryOnce("tearDown('" + getName() + "')") == null) {
				resultString += "Error executing predicate \"tearDown/1\".\n";
				failed = true;
			}

			String filename = "";
			String line = "";
			queryResult = session.queryOnce("file_information('" + getName()
					+ "',File,Line)");
			if (queryResult != null) {
				filename = (String) queryResult.get("File");
				line = (String) queryResult.get("Line");
			} else {
				resultString += "Error executing predicate \"file_infomation/3\".\n";
				failed = true;
			}

			if (failed)
				fail(resultString);

			testMessage += "test('" + getName() + "') defined at \n" + filename
					+ ":" + line + ".\nfailed.\n";

			if (kind.equals("true"))
				return;

			if (kind.equals("fail"))
				fail(testMessage + comment);

			else if (kind.equals("exception"))
				throw new RuntimeException("Prolog Exception: " + testMessage
						+ comment);
			else
				throw new RuntimeException("unkown kind:" + kind);
		} finally {
			if (session != null)
				session.dispose();
		}
	}

}



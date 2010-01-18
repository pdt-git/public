/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.internal.punit;

import java.util.Map;

import junit.framework.TestCase;

import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

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
				fail(testMessage);

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

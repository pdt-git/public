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

package org.cs3.pl.parser;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.cs3.pl.common.Util;

public class ElementDataTest extends TestCase implements ProblemCollector {

	private Vector problems = new Vector();

	private int testNumber;

	private String testLabel;

	public ElementDataTest(String string, int i) {
		super(string);
		this.testNumber = i;
		this.testLabel = generateName("test", i, "");
	}

	private PrologCompiler getPrologCompiler() {
		PrologCompiler c = PrologCompilerFactory.create();
		c.setProblemCollector(this);
		return c;
	}

	public void reportProblem(Problem p) {
		problems.add(p);

	}

	public String getName() {
		return testLabel;
	}

	public void reset() {
		problems.clear();

	}

	public void done() {
		;
	}

	private int getProblemNumber() {
		return problems.size();
	}

	private Problem getProblem(int i) {
		return (Problem) problems.get(i);
	}

	public static Test suite() {
		TestSuite s = new TestSuite();
		for (int i = 0; i < 4; i++) {

			s.addTest(new ElementDataTest("testIt", i));
		}
		s.setName("ElementDataTest");
		return s;
	}

	public void testIt() throws IOException {
		String inputName = generateName("testdata/testelementdata", testNumber,
				".pl");
		String expectName = generateName("testdata/testelementdata",
				testNumber, "-expected.pl");
		InputStream stream = ElementDataTest.class
				.getResourceAsStream(inputName);
		PrologCompiler plc = getPrologCompiler();
		ByteArrayOutputStream buf = new ByteArrayOutputStream();
		Util.copy(stream, buf);
		ByteArrayInputStream in = new ByteArrayInputStream(buf.toByteArray());
		plc.compile(testLabel, in, new StringLineBreakInfoProvider(buf
				.toString()));
		Set publicModulePredicates = plc.getPublicModulePredicates();
		for (Iterator iter = publicModulePredicates.iterator(); iter.hasNext();) {
			String element = (String) iter.next();
			System.out.println(element);
		}
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		plc.saveMetaDataForClauses(out);
		ByteArrayOutputStream expect = new ByteArrayOutputStream();
		stream = ElementDataTest.class.getResourceAsStream(expectName);
		Util.copy(stream, expect);
		assertEquals(expect.toString(), out.toString());
	}

	private static String generateName(String prefix, int n, String sufix) {
		int desiredLength = 3;
		String number = String.valueOf(n);
		int padLength = desiredLength - number.length();
		StringBuffer sb = new StringBuffer(prefix);
		for (int i = 0; i < padLength; i++)
			sb.append('0');
		sb.append(number);
		sb.append(sufix);
		return sb.toString();
	}
}

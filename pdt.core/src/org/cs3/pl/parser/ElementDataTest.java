package org.cs3.pl.parser;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import org.cs3.pl.common.Util;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

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

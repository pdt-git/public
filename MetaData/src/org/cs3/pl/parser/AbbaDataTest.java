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

public class AbbaDataTest extends TestCase implements ProblemCollector {

	private Vector problems = new Vector();

	private int testNumber=0;

	private String testLabel="AbbaDataTest";
	public AbbaDataTest(String string){
		this(string,0);
		testLabel="testIt";
	}
	public AbbaDataTest(String string, int i) {
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

	public static Test _suite() {
		TestSuite s = new TestSuite();
		for (int i = 0; i < 1; i++) {

			s.addTest(new AbbaDataTest("testIt", i));
		}
		s.setName("AbbaDataTest");
		return s;
	}

	public void testIt() throws IOException {
		String inputName = generateName("testdata/testabbadata", testNumber,
				".pl");
		String expectName = generateName("testdata/testabbadata",
				testNumber, "-expected.pl");
		InputStream stream = AbbaDataTest.class
				.getResourceAsStream(inputName);
		PrologCompiler plc = getPrologCompiler();
		ByteArrayOutputStream buf = new ByteArrayOutputStream();
		Util.copy(stream, buf);
		ByteArrayInputStream in = new ByteArrayInputStream(buf.toByteArray());
		plc.compile(testLabel, in, new StringLineBreakInfoProvider(buf
				.toString()));
		
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		plc.saveAbbaData(out);
		ByteArrayOutputStream expect = new ByteArrayOutputStream();
		stream = AbbaDataTest.class.getResourceAsStream(expectName);
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
	
	public static void main(String[] args) throws IOException {
		AbbaDataTest test = new AbbaDataTest("testIt");
		test.testIt();
	}
}

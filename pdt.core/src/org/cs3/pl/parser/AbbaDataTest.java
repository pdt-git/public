package org.cs3.pl.parser;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class AbbaDataTest extends TestCase implements ProblemCollector {

	private Vector problems = new Vector();

	private int testNumber=0;

	private String testLabel="AbbaDataTest";

	private static PrologInterface pif;
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

	public static Test suite() {
		TestSuite s = new TestSuite();
		for (int i = 0; i < 1; i++) {

			s.addTest(new AbbaDataTest("testIt", i));
		}
		s.setName("AbbaDataTest");
		return s;
	}

	public void testIt() throws IOException {
		if(pif==null){
			pif=PrologInterfaceFactory.newInstance().create();
		}
		if(pif.isDown()){
			pif.start();
		}
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
		PrintStream out = pif.getConsultService("").getOutputStream(testLabel);
		//PrintStream out =System.out;
		out.println(":- module("+testLabel+",["+testLabel+"/1]).");
		stream=AbbaDataTest.class.getResourceAsStream("testdata/utils.pl");
		Util.copy(stream, out);
		plc.saveAbbaData(out);
		stream = AbbaDataTest.class.getResourceAsStream(expectName);
		Util.copy(stream, out);
		out.close();
		PrologSession session = pif.getSession();
		StringBuffer sb = new StringBuffer();
		String query=("catch(("+testLabel+"("+testLabel+"),A=ok),E,A=E)");
		try{
			Map r = session.queryOnce(query);
			assertNotNull(r);
			assertEquals("ok",r.get("A").toString());
		}
		finally{
			if(session!=null){
				session.dispose();
			}
		}
		
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

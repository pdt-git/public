package org.cs3.pl.parser;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.cs3.pl.common.Util;

import org.cs3.pl.parser.Index;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.parser.PrologCompilerFactory;
import org.cs3.pl.parser.StringLineBreakInfoProvider;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class IndexerTest extends TestCase {
	public void testIt() throws IOException {
		String inputName = generateName("testdata/testindexer", testNumber,
				".pl");
//		String expectName = generateName("testdata/testindexer",
//				testNumber, "-references.pl");
		InputStream stream = IndexerTest.class
				.getResourceAsStream(inputName);
		PrologCompiler plc = getPrologCompiler();
		ByteArrayOutputStream buf = new ByteArrayOutputStream();
		Util.copy(stream, buf);
		ByteArrayInputStream in = new ByteArrayInputStream(buf.toByteArray());
		plc.compile(testLabel, in, new StringLineBreakInfoProvider(buf
				.toString()));
		Index index=new Index(){
			Set s = new HashSet();
			public void addReference(String key, String filename) {
				if(!s.contains(key)){
					System.out.println(key);
					s.add(key);
				}
				
			}

			public void removeReference(String key, String filename) {
				
			}

			public void removeAllReferences(String filename) {
			}
			public Set getReferringFiles(String key) {
				return null;
			}

			public Set getReferencedKeys(String filename) {
				return null;
			}
			
		};
		plc.updateIndex(index);
	}
	
	private int testNumber;

	private String testLabel;

	public IndexerTest(String string, int i) {
		super(string);
		this.testNumber = i;
		this.testLabel = generateName("test", i, "");
	}

	private PrologCompiler getPrologCompiler() {
		PrologCompiler c = PrologCompilerFactory.create();
		
		return c;
	}
	public String getName() {
		return testLabel;
	}
	
	public static Test suite() {
		TestSuite s = new TestSuite();
		for (int i = 0; i < 1; i++) {

			s.addTest(new IndexerTest("testIt", i));
		}
		s.setName("ElementDataTest");
		return s;
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

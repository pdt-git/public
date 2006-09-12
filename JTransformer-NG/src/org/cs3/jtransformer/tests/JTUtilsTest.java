package org.cs3.jtransformer.tests;

import java.util.HashMap;
import java.util.Map;

import org.cs3.jtransformer.util.JTUtils;

import junit.framework.TestCase;

/**
 * 
 * @author Mark Schmatz
 *
 */
public class JTUtilsTest extends TestCase
{
	public void testAdaptFile()
	{
		String content = "agregoiuh pattern=\"test123\"";
		String expContent = "agregoiuh pattern=\"test123|.*\\.pl|.*\\.aj\"";
		Map regexPatternsWithNewStrings = new HashMap();
		regexPatternsWithNewStrings.put(
				"pattern=\"(.*?)\"",
				"pattern=\"" +
				"${CAPT_GROUP=1}" +
				"|.*\\\\.pl" +
				"|.*\\\\.aj" +
				"\""
		);
		String newContent = JTUtils.adaptContent(content, regexPatternsWithNewStrings);
		assertEquals(newContent, expContent);
		
		// ---
		
		content = "TestlllTest";
		expContent = "Test123lll321Test";
		regexPatternsWithNewStrings = new HashMap();
		regexPatternsWithNewStrings.put(
				"Test(.*?)Test",
				"Test123"+
				"${CAPT_GROUP=1}" +
				"321Test"
		);
		newContent = JTUtils.adaptContent(content, regexPatternsWithNewStrings);
		assertEquals(newContent, expContent);
	}
}

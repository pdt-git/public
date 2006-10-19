package org.cs3.jtransformer.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.TestCase;

public class FileAdaptationHelperTest extends TestCase {
	public void testPatterns() throws Exception {
		Pattern pattern = Pattern.compile("(asdf)[a-zA-Z_]", Pattern.DOTALL);
		Matcher matcher = pattern.matcher("asdfb");
		assertTrue(matcher.find());
		assertEquals("asdf",matcher.group(1));
		matcher = pattern.matcher("asdf/");
		assertFalse(matcher.find());

	}
}

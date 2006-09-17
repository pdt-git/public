package org.cs3.jtransformer.tests;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.jtransformer.util.FileAdaptationHelper;
import org.cs3.jtransformer.util.JTConstants;
import org.cs3.jtransformer.util.JTUtils;

import junit.framework.TestCase;

/**
 * 
 * @author Mark Schmatz
 *
 */
public class SchmatzTest extends TestCase
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
		String newContent = FileAdaptationHelper.adaptContent(content, regexPatternsWithNewStrings);
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
		newContent = FileAdaptationHelper.adaptContent(content, regexPatternsWithNewStrings);
		assertEquals(newContent, expContent);
		
		// ---
		
		content = "<classpathentry including=\"**/*.java|**/*.pl\"";
		expContent = "<classpathentry including=\"**/*.java|**/*.pl|**/cts.list|**/fqcns.list\"";
		regexPatternsWithNewStrings = new HashMap();
		regexPatternsWithNewStrings.put(
				"\\<classpathentry\\s+?including=\"(.*?)\"",
				"<classpathentry including=\"" +
				"${CAPT_GROUP=1}" +
				"|**/cts.list" +
				"|**/fqcns.list" +
				"\""
		);
		newContent = FileAdaptationHelper.adaptContent(content, regexPatternsWithNewStrings);
		assertEquals(newContent, expContent);

		// ---

		content =
			"Manifest-Version: 1.0\n" +
			"Bundle-Name: MarksTestAspectBundle\n" +
			"Bundle-SymbolicName: MarksTestAspectBundle\n" +
			"Bundle-Version: 1.0.0\n" +
			"Bundle-Activator: org.cs3.roots.test.schmatz.demo1.Activator\n" +
			"Import-Package: org.osgi.framework\n" +
			"Export-Package: org.cs3.roots.test.schmatz.demo1.aspects,\n" +
			"de.test123\n";

		expContent =
			"Manifest-Version: 1.0\n" +
			"Bundle-Name: MarksTestAspectBundle\n" +
			"Bundle-SymbolicName: MarksTestAspectBundle\n" +
			"Bundle-Version: 1.0.0\n" +
			"Bundle-Activator: org.cs3.roots.test.schmatz.demo1.Activator\n" +
			"Import-Package: org.osgi.framework\n" +
			"Export-Package: "+JTConstants.RESOURCES_FILELISTS_PACKAGE+", org.cs3.roots.test.schmatz.demo1.aspects,\n" +
			"de.test123\n";

		regexPatternsWithNewStrings = new HashMap();
		regexPatternsWithNewStrings.put(
				"Export-Package:(.*)",
				"Export-Package: " +
				JTConstants.RESOURCES_FILELISTS_PACKAGE + "," +
				"${CAPT_GROUP=1}"
		);
		newContent = FileAdaptationHelper.adaptContent(content, regexPatternsWithNewStrings, JTConstants.RESOURCES_FILELISTS_PACKAGE + ",");
		assertEquals(newContent, expContent);
		
		// ---

		content =
			"Manifest-Version: 1.0\n" +
			"Bundle-Name: MarksTestAspectBundle\n" +
			"Bundle-SymbolicName: MarksTestAspectBundle\n" +
			"Bundle-Version: 1.0.0\n" +
			"Bundle-Activator: org.cs3.roots.test.schmatz.demo1.Activator\n" +
			"Import-Package: org.osgi.framework\n" +
			"Export-Package: "+JTConstants.RESOURCES_FILELISTS_PACKAGE+", org.cs3.roots.test.schmatz.demo1.aspects,\n" +
			"de.test123\n";

		expContent =
			"Manifest-Version: 1.0\n" +
			"Bundle-Name: MarksTestAspectBundle\n" +
			"Bundle-SymbolicName: MarksTestAspectBundle\n" +
			"Bundle-Version: 1.0.0\n" +
			"Bundle-Activator: org.cs3.roots.test.schmatz.demo1.Activator\n" +
			"Import-Package: org.osgi.framework\n" +
			"Export-Package: "+JTConstants.RESOURCES_FILELISTS_PACKAGE+", org.cs3.roots.test.schmatz.demo1.aspects,\n" +
			"de.test123\n";

		regexPatternsWithNewStrings = new HashMap();
		regexPatternsWithNewStrings.put(
				"Export-Package:(.*)",
				"Export-Package: " +
				JTConstants.RESOURCES_FILELISTS_PACKAGE + "," +
				"${CAPT_GROUP=1}"
		);
		newContent = FileAdaptationHelper.adaptContent(content, regexPatternsWithNewStrings, JTConstants.RESOURCES_FILELISTS_PACKAGE);
		assertEquals(newContent, expContent);

		// ---
		
		String expContent2 =
			"Manifest-Version: 1.0\n" +
			"Bundle-Name: MarksTestAspectBundle\n" +
			"Bundle-SymbolicName: MarksTestAspectBundle\n" +
			"Bundle-Version: 1.0.0\n" +
			"Bundle-Activator: org.cs3.roots.test.schmatz.demo1.Activator\n" +
			"Import-Package: org.osgi.framework\n" +
			"Export-Package: "+JTConstants.RESOURCES_FILELISTS_PACKAGE+", "+JTConstants.RESOURCES_FILELISTS_PACKAGE+", org.cs3.roots.test.schmatz.demo1.aspects,\n" +
			"de.test123\n";
		newContent = FileAdaptationHelper.adaptContent(content, regexPatternsWithNewStrings);
		assertEquals(newContent, expContent2);
	}
	
	public void testCTPackageExtractor()
	{ 
		List list = new ArrayList();
		list.add("'org/cs3/roots/test/schmatz/demo1/aspects/org/cs3/roots/test/schmatz/demo1/aspects/DemoAspect__default__37_i'(Jp0) ##--## org.cs3.roots.test.schmatz.demo1.aspects.DemoAspect__default__37_i");
		list.add("'org/cs3/roots/test/schmatz/demo1/aspects/org/cs3/roots/test/schmatz/demo1/aspects/Flip_reverese_103_i'(Jp1) ##--## org.cs3.roots.test.schmatz.demo1.aspects.Flip_reverese_103_i");
		
		String exp = JTConstants.RESOURCES_FOLDER + ".org.cs3.roots.test.schmatz.demo1.aspects, "+JTConstants.RESOURCES_FOLDER + ".org.cs3.roots.test.schmatz.demo1.aspects";
			
		String str = JTUtils.getCTPackagesAsCSV(list);
		
		assertEquals(exp, str);
	}
}

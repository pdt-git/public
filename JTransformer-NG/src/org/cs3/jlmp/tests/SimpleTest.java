package org.cs3.jlmp.tests;

import java.io.File;
import java.util.Map;

import org.cs3.jlmp.JLMPPlugin;
import org.cs3.jlmp.natures.JLMPProjectNature;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

public class SimpleTest extends FactGenerationTest {
	/**
	 * @param name
	 */
	public SimpleTest(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	protected void setUp() throws Exception {
		super.setUp();		
		
		install(new String[]{
			"org/cs3/jlmp/tests/TestChild","org/cs3/jlmp/tests/TestParent"});
		
	}
	
	

	public void testDIT(){
		JLMPProjectNature jlmpProject = getTestJLMPProject();
		PrologInterface pif = jlmpProject.getPrologInterface();
		PrologSession s = pif.getSession();
		Map map = s.queryOnce("dIT('TestParent',A)");
		assertEquals("3", map.get("A"));
		s.dispose();
	}

	
}

package org.cs3.jlmp.tests;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;

public class PersistenceTest extends FactGenerationTest {
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jlmp.tests.FactGenerationTest#setUpOnce()
	 */
	public void setUpOnce() throws Exception {
		super.setUpOnce();
		getTestJLMPProject().getPrologInterface();
		ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
		File r = l.resolve("testdata-facts.zip");
		Util.unzip(r);
		setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator(
				"testdata-facts"));
		setAutoBuilding(false);

		install("rumpel");

	}

	/**
	 * @param name
	 */
	public PersistenceTest(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	public void setUp() throws Exception {
		super.setUp();
		setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator(
		"testdata-facts"));
		getTestJLMPProject().getPrologInterface().getConsultService(JLMP.SRC).clearRecords();
		getTestJLMPProject().getPrologInterface().getConsultService(JLMP.EXT).clearRecords();
	}

	/* (non-Javadoc)
	 * @see org.cs3.jlmp.tests.SuiteOfTestCases#tearDown()
	 */
	protected void tearDown() throws Exception {
		// TODO Auto-generated method stub
		super.tearDown();
		getTestJLMPProject().getPrologInterface().getConsultService(JLMP.SRC).clearRecords();
		getTestJLMPProject().getPrologInterface().getConsultService(JLMP.EXT).clearRecords();
	}
	/**
	 * @throws CoreException
	 * @throws IOException
	 *  
	 */
	public void testRestart() throws Throwable {
		build();
		PrologInterface pif = getTestJLMPProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
		pif.stop();

		pif.start();
		session = pif.getSession();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
	}
}

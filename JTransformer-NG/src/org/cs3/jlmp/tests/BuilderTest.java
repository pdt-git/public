package org.cs3.jlmp.tests;

import java.io.ByteArrayInputStream;
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
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaProject;

public class BuilderTest extends FactGenerationTest {
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

		
		getTestJLMPProject().getPrologInterface().getConsultService(JLMP.SRC).clearRecords();
		getTestJLMPProject().getPrologInterface().getConsultService(JLMP.EXT).clearRecords();
	}

	/**
	 * @param name
	 */
	public BuilderTest(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	public void setUp() throws Exception {
		super.setUp();
		setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator(
		"testdata-facts"));
		PrologInterface pif = getTestJLMPProject().getPrologInterface();
        pif.getConsultService(JLMP.SRC).clearRecords();
		pif.getConsultService(JLMP.EXT).clearRecords();
		pif.start();
	}

	/* (non-Javadoc)
	 * @see org.cs3.jlmp.tests.SuiteOfTestCases#tearDown()
	 */
	protected void tearDown() throws Exception {
		// TODO Auto-generated method stub
		super.tearDown();
		PrologInterface pif = getTestJLMPProject().getPrologInterface();
        pif.getConsultService(JLMP.SRC).clearRecords();
		pif.getConsultService(JLMP.EXT).clearRecords();
		pif.stop();
	}
	/**
	 * @throws CoreException
	 * @throws IOException
	 *  
	 */
	public void testRestart() throws Throwable {	    
	    PrologInterface pif = getTestJLMPProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		assertTrue(pif.getConsultService(JLMP.EXT).isRecording());
		assertTrue(pif.getConsultService(JLMP.SRC).isRecording());
		assertTrue(pif.getConsultService(JLMP.EXT).isAppendingRecords());
		assertFalse(pif.getConsultService(JLMP.SRC).isAppendingRecords());
		build();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
		pif.stop();

		pif.start();
		session = pif.getSession();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		
		uninstall("rumpel");
		build();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
		pif.stop();

		pif.start();
		session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
		
	}
	
	public void testClean()throws Throwable{
	    PrologInterface pif = getTestJLMPProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		build();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		build(IncrementalProjectBuilder.CLEAN_BUILD);
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		
	}
	
	public void testAddAndRemoveToplevels() throws Throwable{
	    PrologInterface pif = getTestJLMPProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		build();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		assertNull(session.queryOnce("classDefT(_,_,'OinkOink',_)"));
		StringBuffer sb = new StringBuffer();
		sb.append("package rumpel;");
		sb.append("public class OinkOink extends Humpel{");
		sb.append("   public int theAnswerToLifeUnivierseAndEverything(){");
		sb.append("   	return 42;");
		sb.append("   }");
		sb.append("}");
		ByteArrayInputStream contents = new ByteArrayInputStream(sb.toString().getBytes());
		IFile file = getTestProject().getFolder("rumpel").getFile("OinkOink.java");
		file.create(contents,true,null);
		build(IncrementalProjectBuilder.INCREMENTAL_BUILD);
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'OinkOink',_)"));
		uninstall("rumpel");
	}
	
	public void testRemoveToplevels() throws Throwable{
	    PrologInterface pif = getTestJLMPProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		build();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		IFile file = getTestProject().getFolder("rumpel").getFile("Humpel.java");
		file.delete(true,null);
		build();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		
	}
	
	public void testMoveToplevels() throws Throwable{
	    PrologInterface pif = getTestJLMPProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		build();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		IJavaProject javaProject = getTestJavaProject();
		StringBuffer sb = new StringBuffer();
		sb.append("package rumpel;");
		sb.append("class Oink extends Humpel{");
		sb.append("   public int theAnswerToLifeUnivierseAndEverything(){");
		sb.append("   	return 42;");
		sb.append("   }");
		sb.append("}");
		ByteArrayInputStream contents = new ByteArrayInputStream(sb.toString().getBytes());
		IFile file = getTestProject().getFolder("rumpel").getFile("OinkOink.java");
		file.create(contents,true,null);
		String oldPath = file.getFullPath().toString();
		build(IncrementalProjectBuilder.INCREMENTAL_BUILD);
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Oink',_)"));		
        assertNotNull(session.queryOnce("toplevelT(_,_,'"+ oldPath+"',_)"));
        
		file.move(new Path("OinkOinkOink.java"),true,null);
		assertFalse(file.exists());
		file = getTestProject().getFolder("rumpel").getFile("OinkOinkOink.java");
		assertTrue(file.exists());
		String newPath=file.getFullPath().toString();
		assertNull(session.queryOnce("toplevelT(_,_,'"+ newPath+"',_)"));
		build(IncrementalProjectBuilder.INCREMENTAL_BUILD);
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Oink',_)"));		
        assertNull(session.queryOnce("toplevelT(_,_,'"+ oldPath+"',_)"));
		assertNotNull(session.queryOnce("toplevelT(_,_,'"+ newPath+"',_)"));
	}
	
	public void _testChangeToplevels() throws Throwable{
	    fail("Test not implemented");
	}
}

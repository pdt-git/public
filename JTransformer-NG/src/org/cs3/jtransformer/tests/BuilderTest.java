package org.cs3.jtransformer.tests;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.cs3.jtransformer.JTDebug;
import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
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
	 * @see org.cs3.jtransformer.tests.FactGenerationTest#setUpOnce()
	 */
	public void setUpOnce() throws Exception {
		setAutoBuilding(false);
	    super.setUpOnce();
		getTestJTransformerProject().getPrologInterface();
		ResourceFileLocator l = JTransformerPlugin.getDefault().getResourceLocator("");
		File r = l.resolve("testdata-facts.zip");
		Util.unzip(r);
		setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator(
				"testdata-facts"));
		
		getTestProject().build(IncrementalProjectBuilder.CLEAN_BUILD,null);
		
		
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
		setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator(
		"testdata-facts"));
		PrologInterface pif = getTestJTransformerProject().getPrologInterface();
        
		pif.start();
		getTestProject().build(IncrementalProjectBuilder.CLEAN_BUILD,null);
	}

	/* (non-Javadoc)
	 * @see org.cs3.jtransformer.tests.SuiteOfTestCases#tearDown()
	 */
	protected void tearDown() throws Exception {
		// TODO Auto-generated method stub
		super.tearDown();
		PrologInterface pif = getTestJTransformerProject().getPrologInterface();
		getTestProject().build(IncrementalProjectBuilder.CLEAN_BUILD,null);
		try{
			pif.stop();
		}catch(Throwable t){
			JTDebug.report(t);
		}
	}
	/**
	 * @throws CoreException
	 * @throws IOException
	 *  
	 */
	public void testRestart_with_pef_store() throws Throwable {
		JTDebug.setDebugLevel("DEBUG");
	    JTransformerPlugin.getDefault().setPreferenceValue(JTransformer.PREF_USE_PEF_STORE,"true");
	    PrologInterface pif = getTestJTransformerProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		build();
        
        assertNotNull(session.queryOnce("toplevelT(_,_,_,_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
		pif.stop();

		pif.start();
//      this caused problems in the past:
        //there was a forgotten deleteSourceFacts in the builder
        build();
		session = pif.getSession();
        assertNotNull(session.queryOnce("toplevelT(_,_,_,_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		
		uninstall("rumpel");
		build(IncrementalProjectBuilder.INCREMENTAL_BUILD);
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
	
	
	public void testRestart_without_pef_store() throws Throwable {
	    JTransformerPlugin.getDefault().setPreferenceValue(JTransformer.PREF_USE_PEF_STORE,"false");
	    PrologInterface pif = getTestJTransformerProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		build();
        
        assertNotNull(session.queryOnce("toplevelT(_,_,_,_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
		pif.stop();

		pif.start();
//      this caused problems in the past:
        //there was a forgotten deleteSourceFacts in the builder
        build();
		session = pif.getSession();
        assertNotNull(session.queryOnce("toplevelT(_,_,_,_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		
		uninstall("rumpel");
		build(IncrementalProjectBuilder.INCREMENTAL_BUILD);
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
		pif.stop();

		pif.start();
		session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		session.dispose();
		
	}
	
	public void testClean()throws Throwable{
	    PrologInterface pif = getTestJTransformerProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		build();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		build(IncrementalProjectBuilder.CLEAN_BUILD);
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		//assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
        //ld: the semantic of clean() has changed. External facts are also deleted.
		assertNull(session.queryOnce("classDefT(_,_,'Object',_)"));
	}
	
	public void testAddAndRemoveToplevels() throws Throwable{
	    PrologInterface pif = getTestJTransformerProject().getPrologInterface();
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
	    PrologInterface pif = getTestJTransformerProject().getPrologInterface();
		PrologSession session = pif.getSession();
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		install("rumpel");
		build();
		assertNotNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		IFile file = getTestProject().getFolder("rumpel").getFile("Humpel.java");
		file.delete(true,null);
		build(IncrementalProjectBuilder.INCREMENTAL_BUILD);
		assertNull(session.queryOnce("classDefT(_,_,'Humpel',_)"));
		assertNotNull(session.queryOnce("classDefT(_,_,'Object',_)"));
		
	}
	
	public void testMoveToplevels() throws Throwable{
	    PrologInterface pif = getTestJTransformerProject().getPrologInterface();
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
	public Test _suite(){
	    TestSuite t = new TestSuite();
	    t.addTest(new BuilderTest("testMoveToplevels"));
	    t.addTest(new BuilderTest("testMoveToplevels"));
	    t.addTest(new BuilderTest("testMoveToplevels"));
	    t.addTest(new BuilderTest("testMoveToplevels"));
	    t.addTest(new BuilderTest("testMoveToplevels"));
	    return t;
	}
}

/*
 * Created on 07.05.2004
 * 
 * To change the template for this generated file go to Window - Preferences -
 * Java - Code Generation - Code and Comments
 */
package org.cs3.jtransformer.tests;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.JTransformerProject;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
/**
 * @author lukas
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
public class SelfTest extends FactGenerationTest {
	

	private String testString;

	public SelfTest(String name){
		super(name);
		testString=generateTestString(1);
	}
	
	/**
	 * @param name
	 */
	public SelfTest(String name, String string) {
		super(name);
		testString=string;
	}
	
	
	public void testSourceWorkSpaceAccess() throws IOException {
		//ld:this is the 6th line of one of my favourite songs
		String expected = " our rose-lipped youth is passing by";
		File sourceFile = getTestDataLocator().resolve("LandSlide");
		assertTrue("does not exist: " + sourceFile.getPath(), sourceFile
				.exists());
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new FileReader(sourceFile));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		String line = null;
		for (int lineNr = 0; lineNr < 6; lineNr++) {
			try {
				line = reader.readLine();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}
		assertEquals(expected, line);
	}
	
	public void testCUAccess1()throws JavaModelException{		
		ICompilationUnit sourceUnit = getCompilationUnit( "test0001", "Test.java");
		assertTrue(sourceUnit.exists());
	}
	
	/**
	 * generic compare of fact file (generated vs exoected()
	 * @throws CoreException
	 * @throws IOException
	 * @throws PrologInterfaceException 
	 */
	public void testIt() throws CoreException, IOException, PrologInterfaceException{
	   System.err.println("\n testing: "+testString);
		System.err.print("   Retrieving cu...");		
		ICompilationUnit sourceUnit = getCompilationUnit(testString, "Test.java");
		System.err.println("done");
		
		System.err.print("   writing facts...");
		JTransformerProject jtransformerProject = JTransformerPlugin.getNature( getTestProject());
		AsyncPrologSession session = ((PrologInterface2)jtransformerProject.getPrologInterface()).getAsyncSession();
		try {
			IFile outFile = writeFactsToFile(session,sourceUnit);

			System.err.println("done");
		
			System.err.print("   retrieving expectedFile...");
			IFile expectedFile = getTestProject().getFile(new Path(testString+"/Test.expected.pl"));		
			System.err.println("done");
			
			System.err.print("   reading expected...");		
			String expected = read(expectedFile);
			System.err.println("done");
			
			System.err.print("   reading actual...");
			String actual =read(outFile);
			System.err.println("done");
			
			System.err.print("   comparing...");
			assertEquals(expected,actual);
			System.err.println("done");
		} finally {
			if(session != null)
				session.dispose();
		}

	}
	
	protected Object getKey() {		
		return SelfTest.class;
	}
	
	public void setUpOnce() throws Exception {	
		super.setUpOnce();
		//no autobuilds please!
		setAutoBuilding(false);
		ResourceFileLocator l = JTransformerPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-selftest.zip");
        Util.unzip(r);
        setTestDataLocator(l.subLocator("testdata-selftest"));
        try {
            install(new String[]{
                    "test0001"
            });
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
		System.err.println("setUpOnce caled for key  "+getKey());
		
	}
	/* (non-Javadoc)
     * @see org.cs3.jtransformer.tests.SuiteOfTestCases#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator("testdata-selftest"));
    }
	
	
	
    /**
	 * @param i
	 * @return
	 */
	private static String generateTestString(int n) {
		int desiredLength=4;
		String number =  String.valueOf(n);
		int padLength = desiredLength -number.length();
		StringBuffer sb = new StringBuffer("test");
		for(int i=0;i<padLength;i++) sb.append('0');
		sb.append(number);
		return sb.toString();
	}
	
	
}
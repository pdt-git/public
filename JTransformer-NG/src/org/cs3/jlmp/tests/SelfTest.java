/*
 * Created on 07.05.2004
 * 
 * To change the template for this generated file go to Window - Preferences -
 * Java - Code Generation - Code and Comments
 */
package org.cs3.jlmp.tests;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
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
	 */
	public void testIt() throws CoreException, IOException{
	   System.err.println("\n testing: "+testString);
		System.err.print("   Retrieving cu...");		
		ICompilationUnit sourceUnit = getCompilationUnit(testString, "Test.java");
		System.err.println("done");
		
		System.err.print("   writing facts...");
		IFile outFile = writeFactsToFile(sourceUnit);
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
	}
	
	protected Object getKey() {		
		return SelfTest.class;
	}
	
	public void setUpOnce() {	
		super.setUpOnce();
		//no autobuilds please!
		setAutoBuilding(false);
		ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
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
     * @see org.cs3.jlmp.tests.SuiteOfTestCases#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator("testdata-selftest"));
    }
	
	
	public static Test _suite() {
        TestSuite s = new TestSuite();      
        s.addTest(new SelfTest("testSourceWorkSpaceAccess"));
        s.addTest(new SelfTest("testCUAccess1"));
        for(int i=1;i<10;i++) s.addTest(new SelfTest("testIt",generateTestString(i)));        
        return s;
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
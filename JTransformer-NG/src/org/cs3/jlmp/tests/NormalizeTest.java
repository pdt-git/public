/*
 * Created on 24.05.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jlmp.tests;

import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jface.text.BadLocationException;

/**
 * @author lukas
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class NormalizeTest extends FactGenerationTest {

	public NormalizeTest(String name) {
		super(name);
	}
	
	public void testNormalize() throws BadLocationException, CoreException, IOException{
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "selftest", "NormalizeTest.java");
		normalizeCompilationUnit(sourceUnit);
		IFile expectedFile = getFile("/Converter/src/selftest/NormalizeTest.expected");
		IFile file = (IFile)sourceUnit.getCorrespondingResource();
		String expected = read(expectedFile);				
		String actual =read(file);		
		assertEquals(expected,actual);
	}
	
	protected Object getKey() {
		return NormalizeTest.class;
	}
	
	public void setUpOnce() {	
		super.setUpOnce();
		System.err.println("setUpOnce caled for key  "+getKey());
		try {
			setUpJLMPProject("Converter");
		} catch (CoreException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void tearDownOnce() {
		super.tearDownOnce();
		System.err.println("tearDownOnce caled for key  "+getKey());
//		try {
//			deleteProject("Converter");
//		} catch (CoreException e) {
//			e.printStackTrace();
//		}
	}
}

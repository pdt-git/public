/*
 * Created on 24.05.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.io.IOException;

import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
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
		ICompilationUnit sourceUnit = getCompilationUnit("", "NormalizeTest.java");
		normalizeCompilationUnit(sourceUnit);
		IFile expectedFile = getTestProject().getFile(new Path("NormalizeTest.expected"));
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
		//no autobuilds please!
		setAutoBuilding(false);
		ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-selftest.zip");
        Util.unzip(r);
        setTestDataLocator(l.subLocator("testdata-selftest"));
        try {
            install(new String[]{
                    "NormalizeTest.expected",
                    "NormalizeTest.java"
            });
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
		System.err.println("setUpOnce caled for key  "+getKey());
		
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

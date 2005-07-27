/*
 * Created on 24.05.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jtransformer.tests;

import java.io.File;
import java.io.IOException;

import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jface.text.BadLocationException;

/**
 * @author lukas
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
public class NormalizeTest extends FactGenerationTest {

    public NormalizeTest(String name) {
        super(name);
    }

    public void testNormalize() throws BadLocationException, CoreException,
            IOException {
        ICompilationUnit sourceUnit = getCompilationUnit("normalize",
                "NormalizeTest.java");
        normalizeCompilationUnit(sourceUnit);
        IFile expectedFile = getTestProject().getFolder("normalize").getFile(
                new Path("NormalizeTest.expected"));
        IFile file = (IFile) sourceUnit.getCorrespondingResource();
        String expected = read(expectedFile);
        String actual = read(file);
        assertEquals(expected, actual);
    }

    protected Object getKey() {
        return NormalizeTest.class;
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
            install("normalize");
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
        System.err.println("setUpOnce caled for key  " + getKey());

    }

    public void testConvertVariableDeclarationFragments() throws Throwable {
        ICompilationUnit sourceUnit = getCompilationUnit("normalize",
                "VariableDeclarationFragments.java");
        normalizeCompilationUnit(sourceUnit);
        IFile expectedFile = getTestProject().getFolder("normalize").getFile(
                new Path("VariableDeclarationFragments.expected"));
        IFile file = (IFile) sourceUnit.getCorrespondingResource();
        String expected = read(expectedFile);
        String actual = read(file);
        assertEquals(expected, actual);
    }

    public void testRemoveNoOps() throws Throwable {
        ICompilationUnit sourceUnit = getCompilationUnit("normalize",
                "NoOps.java");
        normalizeCompilationUnit(sourceUnit);
        IFile expectedFile = getTestProject().getFolder("normalize").getFile(
                new Path("NoOps.expected"));
        
        IFile file = (IFile) sourceUnit.getCorrespondingResource();
        String expected = read(expectedFile);
        String actual = read(file);
        assertEquals(expected, actual);
    }

    public void tearDownOnce() {
        super.tearDownOnce();
        System.err.println("tearDownOnce caled for key  " + getKey());
        //		try {
        //			deleteProject("Converter");
        //		} catch (CoreException e) {
        //			e.printStackTrace();
        //		}
    }
}

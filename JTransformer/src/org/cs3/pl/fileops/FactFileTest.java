package org.cs3.pl.fileops;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.Writer;

import junit.framework.TestCase;

import org.cs3.pl.PDTPlugin;
import org.eclipse.core.resources.IProject;


public class FactFileTest extends TestCase {
	
	public void testCreateFactFile(){
		IProject project = PDTPlugin.getDefault().getActiveFile().getProject();
		FactFile ff = new FactFile(project, "Testfacts");
		FactFile ff2 = new FactFile(project, "TestFacts2", true);
		
		assertEquals (ff.isCompressed(), FactFile.getCompression());
		assertTrue(ff2.isCompressed());
	}
	
	public void testCompresedIO() throws Exception {
		String write = "TESTSTRING";
		IProject project = PDTPlugin.getDefault().getActiveFile().getProject();
		FactFile ff = new FactFile(project, "TestFactFileC", true);
		
		Writer out = ff.getWriter();
		
		out.write(write);
		out.flush();
		out.close();
		
		BufferedReader in = new BufferedReader(ff.getReader());
		
		assertEquals(write, in.readLine());
	}
	
	public void testPlainIO() throws Exception {
		String write = "TESTSTRING";
		IProject project = PDTPlugin.getDefault().getActiveFile().getProject();
		FactFile ff = new FactFile(project, "TestFactFileP", false);
		
		Writer out = ff.getWriter();
		
		out.write(write);
		out.flush();
		out.close();
		
		BufferedReader in = new BufferedReader(ff.getReader());
		
		assertEquals(write, in.readLine());
	}
	
	public void testOutputToStream() throws Exception {
		String write = "TESTSTRING";
		IProject project = PDTPlugin.getDefault().getActiveFile().getProject();
		FactFile ff = new FactFile(project, "TestFactFileS", false);
		
		Writer out = ff.getWriter();
		
		out.write(write);
		out.flush();
		out.close();
		
		ByteArrayOutputStream bout = new ByteArrayOutputStream();
		
		ff.outputContentsToStream(bout);
		
		assertEquals(write, bout.toString());
	}
	
	public void testDelete() throws Exception {
		String write = "TESTSTRING";
		IProject project = PDTPlugin.getDefault().getActiveFile().getProject();
		FactFile ff = new FactFile(project, "TestFactFileD");
		
		Writer out = ff.getWriter();
		
		out.write(write);
		out.flush();
		out.close();
		
		assertTrue(ff.exists());
		
		ff.delete();
		
		assertFalse(ff.exists());
	}
}

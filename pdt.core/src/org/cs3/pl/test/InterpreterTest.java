package org.cs3.pl.test;

import java.io.File;
import java.io.FileFilter;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

public class InterpreterTest extends TestCase {

	private final File file;
	private PrologInterface pif;
	public InterpreterTest(String name, File file) {
		super(name);
		this.file = file;

	}
	
	public String getName() {
		
		
		return file.getAbsolutePath();
	}
	protected void setUp() throws Exception {
		super.setUp();
		String[] codebase = System.getProperty("codebase").split(System.getProperty("path.separator"));
		pif = AbstractPrologInterface.newInstance();
		PrologSession s = pif.getSession();
		for (int i = 0; i < codebase.length; i++) {
			s.queryOnce("assert(file_search_path(library,'"+codebase[i]+"'))");	
		}
		s.queryOnce("use_module(library('builder/targets/test_interpreter'))");
		s.queryOnce("nospyall");
		s.dispose();
	}
	
	protected void tearDown() throws Exception {
		super.tearDown();
		pif.stop();
		pif=null;
	}
	
	public void testIt()throws Throwable{
		PrologSession s = pif.getSession();
		List<Map<String,Object>> list = s.queryAll("pdt_test_interpreter('"+Util.prologFileName(file)+"',R)");
		s.dispose();
		for (Iterator<Map<String,Object>> iter = list.iterator(); iter.hasNext();) {
			Map<String,Object> map = iter.next();
			String result=(String) map.get("R");
			assertFalse(result,result.startsWith("failed"));
		}
	}
	
	public static Test suite() {

		TestSuite s = new TestSuite();
		String testdata = System.getProperty("testdata");
		if(testdata==null){
			throw new RuntimeException("Please define system property \"testdata\". \n " +
					"It should be the root of the directory tree containging your test data.");
		}
		String codebase = System.getProperty("codebase");
		if(codebase==null){
			throw new RuntimeException("Please define system property \"codebase\". \n " +
					"It should be a list of search path entries separated by your systems path separator.");
		}
		File root = new File(testdata);
		LinkedList<File> todo = new LinkedList<File>();
		todo.add(root);
		FileFilter filter = new FileFilter() {
			public boolean accept(File f) {
				return f.isDirectory() || f.getName().equals("main.pl");
			}
		};
		while (!todo.isEmpty()) {
			File dir = todo.removeFirst();
			File[] files = dir.listFiles(filter);
			Arrays.sort(files);
			for (int i = 0; i < files.length; i++) {
				File file = files[i];
				if (file.isDirectory()) {
					todo.add(file);
				} else {
					InterpreterTest test = new InterpreterTest("testIt", file);

					s.addTest(test);
				}
			}
		}

		return s;
	}
}

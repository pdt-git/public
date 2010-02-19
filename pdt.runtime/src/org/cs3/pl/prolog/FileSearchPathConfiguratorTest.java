package org.cs3.pl.prolog;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

public class FileSearchPathConfiguratorTest {
	PrologLibraryManager mgr;
	
	DummyPrologLibrary a = new DummyPrologLibrary("a","");
	DummyPrologLibrary b = new DummyPrologLibrary("b", "a");
	DummyPrologLibrary c = new DummyPrologLibrary("c", "");
	DummyPrologLibrary d = new DummyPrologLibrary("d","ac");
	DummyPrologLibrary e = new DummyPrologLibrary("e","b");
	
	@Before
	public void setUp() {
		mgr = new PrologLibraryManager();
	}
	
	@Test
	public void testGetRequiredLibs_NullMng() {
		String[] libs = {"a"};
		try {
			FileSearchPathConfigurator.getRequiredLibs(null, libs);
			fail("an exception should occure");
		}catch(NullPointerException e) {
		}
	}

	@Test
	public void testGetRequiredLibs_NullLibs() {
		try {
			FileSearchPathConfigurator.getRequiredLibs(mgr, null);
			fail("an exception should occure");
		}catch(NullPointerException e) {
		}
	}
	
	@Test
	public void testGetRequiredLibs_NoLibsNeeded() {
		String[] libs = {"a"};
		mgr.addLibrary(a);
		PrologLibrary[] result=FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
		PrologLibrary[] expected={a};
		assertArrayEquals(expected,result);
	}
	
	@Test
	public void testGetRequiredLibs_OneNeededIsThere() {
		String[] libs = {"b"};
		mgr.addLibrary(a);
		mgr.addLibrary(b);
		PrologLibrary[] result=FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
		PrologLibrary[] expected={b,a};
		List<PrologLibrary> resultList = Arrays.asList(result);
		List<PrologLibrary> expectedList = Arrays.asList(expected);
		assertEquals(expectedList.size(),resultList.size());
		assertTrue(resultList.containsAll(expectedList));
	}
	
	@Test
	public void testGetRequiredLibs_TwoNeededAreThere() {
		String[] libs = {"d"};
		mgr.addLibrary(a);
		mgr.addLibrary(c);
		mgr.addLibrary(d);
		PrologLibrary[] result=FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
		PrologLibrary[] expected={d,a,c};
		List<PrologLibrary> resultList = Arrays.asList(result);
		List<PrologLibrary> expectedList = Arrays.asList(expected);
		assertEquals(expectedList.size(),resultList.size());
		assertTrue(resultList.containsAll(expectedList));
	}
	
	@Test
	public void testGetRequiredLibs_OneNeededDirectlyOneNeededIndirectlyBothAreThere() {
		String[] libs = {"e"};
		mgr.addLibrary(a);
		mgr.addLibrary(b);
		mgr.addLibrary(e);
		PrologLibrary[] result=FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
		PrologLibrary[] expected={e,b,a};
		List<PrologLibrary> resultList = Arrays.asList(result);
		List<PrologLibrary> expectedList = Arrays.asList(expected);
		assertEquals(expectedList.size(),resultList.size());
		assertTrue(resultList.containsAll(expectedList));
	}
	
	@Test
	public void testGetRequiredLibs_LibNotThere() {
		String[] libs = {"a"};
		try {
			FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
			fail("an exception should occure");
		} catch(IllegalArgumentException e) {
			assertEquals("library id a is unresolved", e.getMessage());
		}
	}

	@Test
	public void testGetRequiredLibs_RequiredLibNotThere() {
		String[] libs = {"b"};
		mgr.addLibrary(b);
		try {
			FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
			fail("an exception should occure");
		} catch(IllegalArgumentException e) {
			assertEquals("library id b is broken", e.getMessage());
		}
	}
	
	@Test
	public void testGetRequiredLibs_WrongLibId() {
		String[] libs = {"x"};
		try {
			FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
			fail("an exception should occure");
		} catch(IllegalArgumentException e) {
			assertEquals("library id x is unresolved", e.getMessage());
		}
	}
	
}

package org.cs3.pl.metadata.tests;

import java.util.HashMap;

import org.cs3.pl.parser.PrologCompiler;

import junit.framework.TestCase;

public class PrologCompilerTest extends TestCase {

	
	public void test_PDT_34() {
		PrologCompiler compiler = new PrologCompiler();
		compiler.compile(":- module(knarz,[]),\ngut(ich).");
		HashMap map = compiler.getPublicModulePredicates();
		assertNotNull(map);
		assertEquals(0,map.size());
	}

}

package org.cs3.pl.metadata.tests;

import java.util.Set;

import junit.framework.TestCase;

import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.parser.PrologCompilerFactory;

public class PrologCompilerTest extends TestCase {

	
	public void test_PDT_34() {
		PrologCompiler compiler = PrologCompilerFactory.create();
		compiler.compile(":- module(knarz,[]),\ngut(ich).");
		Set set = compiler.getPublicModulePredicates();
		assertNotNull(set);
		assertEquals(0,set.size());
	}

}

package org.cs3.pl.parser;

import org.cs3.pl.parser.internal.term.TermBasedPrologCompiler;

public class PrologCompilerFactory {
	public static PrologCompiler create(){
		return new TermBasedPrologCompiler();
	}
}

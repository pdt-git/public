package org.cs3.jlmp.astvisitor;

import org.cs3.jlmp.bytecode.IDManagerIType;
import org.cs3.pl.prolog.PrologSession;


/**
 * the default set of helper classes for Java->Prolog fact generation.
 * 
 * @inheritDoc
 * @author schulzs1
 */
public class DefaultGenerationToolbox extends FactGenerationToolBox {
	
	/**
	 * creates a new GenerationToolbox, with default Options. This means
	 * local IDs, and a local table for fqn()-Terms.
	 */
	
	public DefaultGenerationToolbox(PrologSession client) {
		provider = new LocalIDGenerator();
		fqntrans = new LocalIDFQNTranslator((LocalIDGenerator)provider);
		idresolver = new IDResolver(fqntrans, provider);
		tresolver = new TypeResolver(fqntrans);
		manager = new IDManagerIType(client, fqntrans, provider);
	}
}

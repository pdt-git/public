package org.cs3.jlmp.internal.astvisitor;

import org.cs3.jlmp.internal.bytecode.IDManagerIType;


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
	
	public DefaultGenerationToolbox() {
		provider = new LocalIDGenerator();
		fqntrans = new LocalIDFQNTranslator((LocalIDGenerator)provider);
		idresolver = new IDResolver(fqntrans, provider);
		tresolver = new TypeResolver(fqntrans,idresolver);
		manager = new IDManagerIType(fqntrans, provider);
	}
}

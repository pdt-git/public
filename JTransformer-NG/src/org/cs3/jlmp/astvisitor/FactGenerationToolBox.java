package org.cs3.jlmp.astvisitor;

import org.cs3.jlmp.bytecode.IDManagerIType;

/**
 * Provides the necessary helper objects for fact generation. Should be subclassed
 * for a "strategy" like "Normal Fact Generation", or "Fact Generation for a Testcase.
 * 
 * @author schulzs1
 */
abstract public class FactGenerationToolBox {
	protected FQNTranslator fqntrans;
	protected IIDGenerator provider;
	protected IIDResolver idresolver;
	protected ITypeResolver tresolver;
	protected IDManagerIType manager;
	
	/**
	 * returns the FQNTranslator appropriate for this method of
	 * fact generation.
	 * @return a FQNTranslator object
	 */
	
	public FQNTranslator getFQNTranslator(){
		return fqntrans;
	}
	
	/**
	 * returns the IDGenerator appropriate for this method of 
	 * fact generation
	 * @return an IDGenerator object
	 */
	
	public IIDGenerator getIDGenerator(){
		return provider;
	}
	

	/**
	 * returns the IDResolver appropriate for this method of 
	 * fact generation
	 * @return an IDResolver object
	 */
	
	public IIDResolver getIDResolver() {
		return idresolver;
	}
	

	/**
	 * returns the TypeResolver appropriate for this method of 
	 * fact generation
	 * @return an TypeResolver object
	 */
	
	public ITypeResolver getTypeResolver(){
		return tresolver;
	}

	/**
	 * This method returns an IDManager instance. This is used for byte code fact generation
	 * 
	 * TODO: merge IDManager and IDResolver
	 * 
	 * @return an IDManager instance
	 */
	public IDManagerIType getIDManager() {
		return manager;
	}
	
}

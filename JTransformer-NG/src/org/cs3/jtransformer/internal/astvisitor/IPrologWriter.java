/*
 * Created on 25.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jtransformer.internal.astvisitor;

/**
 * @author windeln
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public interface IPrologWriter {
	/**
	 * Sets the indention level to the first parameter (each indention, one tab
	 * is added before the rule's head)
	 * @param i the new indention level
	 * @throws IllegalArgumentException i is less then 0
	 */
	public abstract void setIndentionLevel(int i);
	/**
	 * increases the indention level by 1
	 *
	 */
	public abstract void addIndention();
	/**
	 * reduces the indention level by 1
	 * @throws IllegalStateException the indention level is already 0, and 
	 * can not be reduced further.
	 */
	public abstract void reduceIndention();
	/**
	 * Writes a fact to the underlying structure, with the name equal to the first
	 * argument, and a number of arguments passed as an array of Strings. If the
	 * array is null or empty, the rule has form string().
	 * @param string the name of the rule
	 * @param param an array of arguments to be included in the rule head
	 */
	public abstract void writeFact(String string, String[] param);
	/**
	 * Writes a rule to the underlying structure, with the name equal to the first
	 * argument, and a number of arguments passed as an array of Strings. Normally
	 * it has the form string(param[0], param[1],...) :- condi[0], ... . If the
	 * array is null or empty, the rule has form string() :- ... <br>
	 * If the  parameter param is null or empty, it has the form string(args) and
	 * constitutes a fact.
	 * @param string the name of the rule
	 * @param param the array of parameters to be passed
	 * @param condi conditions to be included on the right hand side
	 */
	public abstract void writeRule(String string, String[] param, String[] condi);
	
	/**
	 * Closes the underlying structure, and flushes any buffers. Behaviour on further calls
	 * to writeFact() and writeRule is undefined.
	 */
	public abstract void close();
	
	/**
	 * Flushes any buffers
	 */
	public abstract void flush();
	
	/**
	 * Enables intrepretation mode.
	 * <p>
	 * wraps facts with :- inTe(..) predicate.
	 * <p>
	 * <b>N.b.</b> Wether this method actualy does <b>anything</b>
	 * purely depends on the concrete implementation. E.g. the
	 * MockPrologWriter completely ignores this property. 
	 * @param interpret 
	 * @todo this should propably be refactored!
	 */
	public void setInterpretMode(boolean interpret);
	
	/**
	 * wether this writer is in interpretation mode.
	 * @see setInterpretMode(boolean)
	 * @return true if this writer is in interpret mode.
	 */
	public boolean getInterpretMode();
	
	/**
	 * Writes a query to the underlying structure:
	 * :- query.
	 * 
	 * @param query an arbitrary Prolog query. The query must <b>not</b> end 
	 * with a dot ('.'). 
	 */
	public abstract void writeQuery(String query);
	 
}
/*
 * Created on 04.05.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jlmp.internal.astvisitor;

/**
 * Provides a mechanism to generate distinct IDs. Classes implementing
 * this Interface provide a consistant way of generating Strings (IDs)
 * that are used to name different Objects. No two calls to getID should
 * ever return the same result.
 * 
 * @author schulzs1
 * @see org.cs3.jlmp.internal.astvisitor.IIDResolver
 */
public interface IIDGenerator {
	

	/**
	 * returns an unique identifyer that is guaranteed to be different from 
	 * all previously returned and all later returned IDs
	 */
	
	public String getID();
}
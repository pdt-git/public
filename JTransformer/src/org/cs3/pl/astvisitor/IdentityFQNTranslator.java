/*
 * Created on 29.04.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.astvisitor;

import java.util.LinkedList;
import java.util.List;

/**
 * This class simply returns all FQN terms that are passed to it.
 * @author schulzs1
 */
public class IdentityFQNTranslator implements FQNTranslator {

	/** 
	 * returns an empty list.
	 * @see org.cs3.pl.astvisitor.FQNTranslator#getFQNMapping()
	 * @return an empty list
	 */
	public List getFQNMapping() {
		return new LinkedList();
	}

	/**
	 * returns the String passed to it.
	 * @see org.cs3.pl.astvisitor.FQNTranslator#transformFQN(java.lang.String)
	 * @param string a String
	 * @return string
	 */
	public String transformFQN(String string) {
		return string;
	}
}

/*
 * Created on 04.05.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.astvisitor;

/**
 * Provides local IDs. Local IDs have the form lId(NR), with NR being
 * a numerical value. This form is well-suited to be used in inTe, which
 * exchanges them with global IDs.
 * 
 * @author schulzs1
 */
public class LocalIDGenerator implements IIDGenerator {

	private int next;
	
	/**
	 * creates a new LocalIDProvider.
	 */
	
	public LocalIDGenerator(){
		next = 0;
	}
	
	/**
	 * creates a new LocalIDProvider, using <code>first</code> as the 
	 * first number returned.
	 * @param first the first nr returned in an lId(NR) term
	 */
	
	public LocalIDGenerator(int first){
		next = first;
	}
	
	/**
	 * creates a new local ID.
	 * @return a lId(...) term
	 */

	public String getID() {
		return "lId(" + next++ + ")";
	}
}

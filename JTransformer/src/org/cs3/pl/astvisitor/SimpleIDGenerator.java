/*
 * Created on 04.05.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.astvisitor;

/**
 * Provides simple IDs, incrementing a starting number for each call to
 * getID().
 * 
 * @author schulzs1
 */
public class SimpleIDGenerator implements IIDGenerator {
	
	private int next;
	
	/**
	 * constructs a new SimpleIDProvider, starting with ID 0
	 */
	
	public SimpleIDGenerator(){
		next = 0;
	}
	
	/**
	 * constructs a new SimpleIDProvider, starting with a specified first
	 * id
	 * @param first the first ID to be returned
	 */
	
	public SimpleIDGenerator(int first){
		next = first;
	}
	
	/**
	 * returns a string, that is a simple Number. Each call to getID
	 * increments that number by one.
	 */
	
	public String getID() {
		return Integer.toString(next++);
	}
}
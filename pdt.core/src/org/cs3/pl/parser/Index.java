package org.cs3.pl.parser;

import java.util.Set;

public interface Index {
	
	/**
	 * 
	 * @param key index key
	 * @param loc reference position
	 */
	public void addReference(String key, String filename);
	
	/**
	 * 
	 * @param key index key
	 * @param loc reference position
	 */
	public void removeReference(String key, String filename);
	
	public void removeAllReferences(String filename);
	
	/**
	 * 
	 * @param key index key
	 * @return a set of filenames
	 * 				
	 */
	public Set getReferringFiles(String key);
	
	
	/**
	 * 
	 * @param key index file
	 * @return a set of keys
	 * 				
	 */
	public Set getReferencedKeys(String filename);
	
	
	
}

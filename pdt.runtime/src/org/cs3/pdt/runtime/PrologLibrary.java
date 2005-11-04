package org.cs3.pdt.runtime;

import java.util.List;

public interface PrologLibrary {
	/**
	 * @return the global unique identifier for this library
	 */
	public String getId();
	
	/**
	 * 
	 * @return The resolved absolute path (the prolog flavour) of the 
	 * directory that contains the library
	 */
	public String getPath();
	
	/**
	 * @return The alias this library uses. (see file_search_path/2)
	 */
	public String getAlias();
	
	/**
	 * @return a list of identifiers of libraries this library depends on.
	 */
	public List getDependencies();
}

package org.cs3.pdt.runtime;

import java.util.Set;

/**
 * A contribution to the Prolog file lookup path.
 * 
 * An instance moreless represents a clause that can be
 * contributed to the file_search_path/2 predicate.
 * @author lukas
 *
 */
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
	public Set getDependencies();
}

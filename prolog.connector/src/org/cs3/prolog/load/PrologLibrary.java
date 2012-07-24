/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.load;

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
	public Set<String> getDependencies();
	
	/**
	 * retrieve application-specific data from the library.
	 * 
	 * Clients may attach additional, string valued attributes to the library that are relevant 
	 * to the respective application.
	 * The pdt core for example adds a flag "hidden" which is set for all and runtime libraries.
	 *  
	 * @param attr the attribute name
	 * @return the attribute value or null if not set.
	 */
	public String getAttributeValue(String attr);
}


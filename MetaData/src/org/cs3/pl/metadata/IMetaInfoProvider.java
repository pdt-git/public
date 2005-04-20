
package org.cs3.pl.metadata;

import org.cs3.pl.prolog.PrologException;



public interface IMetaInfoProvider {
	/**
	 * Retrieves the location or a predicate in the running prolog prozess.
	 * 
	 * @param functor
	 *            The functor of the predicate.
	 * @param arity
	 *            The arity of the predicate.
	 * @return null, if no location can be found.
	 * @throws PrologException
	 * @see SourceLocation
	 */
    public abstract SourceLocation getLocation(String label, int arity, String contextFile) throws PrologException;    
    
   public abstract PrologElementData[] getPredicatesWithPrefix(String module, String prefix, String activeFileName) throws PrologException;
	
	public abstract PrologElementData[] getPredicatesWithPrefix(String string, String prefix) throws NumberFormatException, PrologException;
	
	public abstract PrologElementData[] retrievePrologElements(String filename) throws PrologException;
    
	public abstract String getHelp(PrologElementData elm);
}

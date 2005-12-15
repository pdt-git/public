
package org.cs3.pl.metadata;

import org.cs3.pl.prolog.PrologException;


/**
 * 
 * @author lukas
 *
 */
public interface IMetaInfoProvider {
	
	public abstract Clause[] findClauses(Predicate p);
    
	public abstract Predicate[] getPredicatesWithPrefix(String module, String prefix, String activeFileName) throws PrologException;
	
	public abstract Predicate[] getPredicatesWithPrefix(String string, String prefix) throws NumberFormatException, PrologException;
	
	public abstract Clause[] retrievePrologElements(String filename) throws PrologException;
    
	public abstract String getHelp(Predicate elm);
	
	public abstract SourceLocation[] findReferences(Predicate data);
	
	public abstract Predicate[] findPredicates(Goal data);
	
	public String getSummary(Predicate data) throws PrologException;
	
	
}

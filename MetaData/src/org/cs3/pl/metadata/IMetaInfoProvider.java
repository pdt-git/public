
package org.cs3.pl.metadata;

import org.cs3.pl.prolog.PrologException;



public interface IMetaInfoProvider {
   
    public abstract SourceLocation getLocation(String label, int arity, String string) throws PrologException;    
    
   public abstract PrologElementData[] getPredicatesWithPrefix(String module, String prefix, String activeFileName) throws PrologException;
	
	public abstract PrologElementData[] getPredicatesWithPrefix(String string, String prefix) throws NumberFormatException, PrologException;
	
	public abstract PrologElementData[] retrievePrologElements(String filename) throws PrologException;
    
	public abstract String getHelp(PrologElementData elm);
}

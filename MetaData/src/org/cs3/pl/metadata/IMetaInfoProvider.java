
package org.cs3.pl.metadata;

import org.cs3.pl.prolog.SessionException;


public interface IMetaInfoProvider {
   
    public abstract SourceLocation getLocation(String label, int arity, String string) throws SessionException;    
    
   public abstract PrologElementData[] getPredicatesWithPrefix(String module, String prefix, String activeFileName) throws SessionException;
	
	public abstract PrologElementData[] getPredicatesWithPrefix(String string, String prefix) throws NumberFormatException, SessionException;
	
	public abstract PrologElementData[] retrievePrologElements(String filename) throws SessionException;
    
}

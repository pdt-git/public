
package org.cs3.pl.prolog;

public interface IMetaInfoProvider {
   
    public abstract SourceLocation getLocation(String label, int arity, String string);    
    
   public abstract PrologElementData[] getPredicatesWithPrefix(String module, String prefix, String activeFileName);
	
	public abstract PrologElementData[] getPredicatesWithPrefix(String string, String prefix);
	
	public abstract PrologElementData[] retrievePrologElements(String filename);
    
}

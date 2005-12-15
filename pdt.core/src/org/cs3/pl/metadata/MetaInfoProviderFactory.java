package org.cs3.pl.metadata;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;


public abstract class MetaInfoProviderFactory {
	public final static String DEFAULT="org.cs3.pl.metadata.internal.classic.Factory";
	public final static String DEFAULT_STATIC="org.cs3.pl.metadata.internal.classic.Factory";
	public final static String DEFAULT_RUNTIME="org.cs3.pl.metadata.internal.classic.Factory";
	public static MetaInfoProviderFactory newInstance(){
        return newInstance(DEFAULT);
    }
    
    public static MetaInfoProviderFactory newInstance(String fqn) {
        try{
            Class impl = Class.forName(fqn);
        
        if(!MetaInfoProviderFactory.class.isAssignableFrom(impl)){
            throw new IllegalArgumentException("not a valid factory class");
        }
        return (MetaInfoProviderFactory) impl.newInstance();
        }
        catch(Throwable t){
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }
    
    public abstract IMetaInfoProvider create(PrologInterface pif);
    
}

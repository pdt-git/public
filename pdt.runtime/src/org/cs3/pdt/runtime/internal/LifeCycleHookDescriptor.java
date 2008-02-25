package org.cs3.pdt.runtime.internal;

import java.io.File;

import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PrologLibraryManager;

/**
 * A description of a life cycle hook as obtained from the plugin.xml file.
 * 
 * Instances can be used to create hook instances
 * 
 * @author degenerl
 * 
 */
public  interface LifeCycleHookDescriptor {
	public String[] getLibraryDependencies();
	public String[] getHookDependencies() ;
	public File[] getConsults() ;
	public String getHookId() ;
	public String[] getTags() ;
	
	
	/**
	 * creates a hook instance parameterized with the given domain object.
	 * 
	 * It is the responsible of returned hook to make sure that the hook
	 * configures the pif according to this description.
	 * See LiveCycleHooiDecorator for a typical implementation.
	 */
	public LifeCycleHook2 createHook(Object data) ;
	
}

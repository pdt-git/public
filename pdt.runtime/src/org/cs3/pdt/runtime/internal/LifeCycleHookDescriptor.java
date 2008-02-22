package org.cs3.pdt.runtime.internal;

import java.io.File;

import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologLibraryManager;

/**
 * A description of a life cycle hook as obtained
 * from the plugin.xml file.
 * 
 * Instances can be used to create hook instances 
 * @author degenerl
 *
 */
public abstract class LifeCycleHookDescriptor {
	LifeCycleHook implementation;
	private PrologLibraryManager libmgr;
	private String[] libraryDependencies;
	private String[] hookDependencies;
	private File[] consults;
	private String[] tags;
	private String hookId;

	
	
	public LifeCycleHookDescriptor(Class implementation,
			PrologLibraryManager libmgr, String[] libraryDependencies,
			String[] hookDependencies, File[] consults, String[] tags,String hookId) {
		super();
		this.hookId = hookId;
		this.libmgr = libmgr;
		this.libraryDependencies = libraryDependencies;
		this.hookDependencies = hookDependencies;
		this.consults = consults;
	}

	public abstract LifeCycleHook getImplementation();

	public PrologLibraryManager getLibmgr() {
		return libmgr;
	}

	public String[] getLibraryDependencies() {
		return libraryDependencies;
	}

	public String[] getHookDependencies() {
		return hookDependencies;
	}

	public File[] getConsults() {
		return consults;
	}

	public LifeCycleHook2 createHook() {
		LifeCycleHookDecorator decoratedHook = new LifeCycleHookDecorator(getImplementation(),
				libraryDependencies, consults, libmgr);			
			return decoratedHook;
		
	}
}

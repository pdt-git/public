package org.cs3.pdt.runtime.internal;

import java.io.File;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

/**
 * A description of a life cycle hook as obtained from the plugin.xml file.
 * 
 * Instances can be used to create hook instances
 * 
 * @author degenerl
 * 
 */
public class LifeCycleHookDescriptor {
	private final PrologRuntimePlugin prologRuntimePlugin;
	private final String[] tags;
	private final File[] consults;
	private final String[] hookDependencies;
	private final String id;
	private final String[] libraryDependencies;
	private final IConfigurationElement celem;

	public LifeCycleHookDescriptor(PrologRuntimePlugin prologRuntimePlugin, String[] tags, File[] consults,
			String[] hookDependencies, String id,
			String[] libraryDependencies, IConfigurationElement celem) {
		this.prologRuntimePlugin = prologRuntimePlugin;
		this.tags = tags;
		this.consults = consults;
		this.hookDependencies = hookDependencies;
		this.id = id;
		this.libraryDependencies = libraryDependencies;
		this.celem = celem;
	}

	
	public LifeCycleHook getImplementation() {
		if (celem.getAttribute("class") == null) {
			try {
				return (LifeCycleHook) celem.createExecutableExtension("class");
			} catch (CoreException e) {
				Debug.rethrow(e);
			}
		}
		return null;
	}
	public LifeCycleHook createHook(Object data) {
		LifeCycleHook hook = new LifeCycleHookProxy(getImplementation(), libraryDependencies, consults, this.prologRuntimePlugin.getLibraryManager());
		hook.setData(data);
		return hook;
	}
	public File[] getConsults() {
		return consults;
	}
	public String[] getHookDependencies() {
		return hookDependencies;
	}
	public String getHookId() {
		return id;
	}
	public String[] getLibraryDependencies() {
		return libraryDependencies;
	}
	public String[] getTags() {
		return tags;
	}
	
}

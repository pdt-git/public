package org.cs3.pdt.core;

import java.util.Set;

import org.cs3.pl.common.OptionProvider;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

/**
 * not much yet ;-)
 */
public interface IPrologProject extends OptionProvider{
    

	    
    
    /**
     * parses the current value of the sourcePath property and returns
     * a Set containing IContainer objects that represent the
     * source folders that actualy exist within the project.
     * @return
     * @throws CoreException
     */
    public Set getExistingSourcePathEntries() throws CoreException;
    
    /**
     * Checks if the given resource is part of the prolog  source.
     * <p>
     * A folder is regarded as part of the source tree if it exists and is either
     * a source path entry itself or its parent is part of the source tree..
     * <p>
     * a file is regarded as part of the source tree if it exists, is the child of a 
     * folder that is part of the source tree and has the ".pl" file extension.
     * @param resource
     * @return
     * @throws CoreException
     */
    public boolean isPrologSource(IResource resource) throws CoreException;
    
    public boolean isAutoConsulted(IFile file) throws CoreException;
    public void setAutoConsulted(IFile file,boolean val) throws CoreException;

	public PrologInterface getMetadataPrologInterface();
	public IMetaInfoProvider getMetaInfoProvider();

	public IProject getProject();

	public PrologInterface getRuntimePrologInterface();
    
    
}

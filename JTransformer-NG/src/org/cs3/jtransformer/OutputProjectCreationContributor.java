package org.cs3.jtransformer;

import java.util.Collection;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

public interface OutputProjectCreationContributor {

	/**
	 * Returns a list of FileAdapationHelper objects.
	 * 
	 * @param srcProject
	 * @param destProject
	 * @param classPathReplacement
	 * @return
	 * @throws CoreException 
	 */
	Collection getFileAdaptionHelpers(IProject srcProject, Map classPathReplacement) throws CoreException;

}

package org.cs3.pdt;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

public final class PDTUtils {

	public static IPath  normalize(IPath path) {
	    IPath testLocation = null;
	    try {
	        
	        testLocation = new Path(path.toFile().getCanonicalFile().toString());
	    } catch (IOException e1) {
	       Debug.report(e1);
	       throw new RuntimeException(e1);
	    }
	    return testLocation;
	}

	/**
	 * adapted from org.eclipse.core.internal.localstore.FileSystemResourceManager.
	 * This is the "corrected" version: it does normalize the locations before comparing 
	 * them. propably hurts performance, but i cant help it. --lu
	 */
	public static List allPathsForLocation(IPath l) {
	    IPath location = normalize(l);
		IProject[] projects =  ResourcesPlugin.getWorkspace().getRoot().getProjects();
		final ArrayList results = new ArrayList();
		for (int i = 0; i < projects.length; i++) {
			IProject project = projects[i];
			//check the project location
			
			
			IPath testLocation = normalize(project.getLocation());
	        IPath suffix;
			if (testLocation != null && testLocation.isPrefixOf(location)) {
				suffix = location.removeFirstSegments(testLocation.segmentCount());
				results.add(project.getFullPath().append(suffix));
			}
			if (!project.isAccessible())
				continue;
			IResource[] children = null;
			try {
				children = project.members();
			} catch (CoreException e) {
				//ignore projects that cannot be accessed
			}
			if (children == null)
				continue;
			for (int j = 0; j < children.length; j++) {
				IResource child = children[j];
				if (child.isLinked()) {
					testLocation = normalize(child.getLocation());
					if (testLocation != null && testLocation.isPrefixOf(location)) {
						//add the full workspace path of the corresponding child of the linked resource
						suffix = location.removeFirstSegments(testLocation.segmentCount());
						results.add(child.getFullPath().append(suffix));
					}
				}
			}
		}
		return results;
	}

	/**
	 * @param file
	 * @return
	 * @throws IOException
	 */
	public static IFile[] findFilesForLocation(String path) {
	    IFile file = null;
	    IWorkspace workspace = ResourcesPlugin.getWorkspace();
	    IWorkspaceRoot root = workspace.getRoot();
	    IPath fpath = new Path(path);
	
	    List list = PDTUtils.allPathsForLocation(fpath);
	    ArrayList result = new ArrayList(list.size());
	    for (Iterator it = list.iterator(); it.hasNext();) {
	        IPath p = (IPath) it.next();
	        IResource r = root.findMember(p);
	        if (r.getType()==IResource.FILE){
	            result.add(r);
	        }
	    }
		IFile[] files = (IFile[]) result.toArray(new IFile[result.size()]);
	    return files;
	}
	
}

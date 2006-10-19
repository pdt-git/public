package org.cs3.jtransformer.internal.actions;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.cs3.jtransformer.internal.natures.JTransformerProjectNature;
import org.cs3.jtransformer.util.JTUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jdt.launching.JavaRuntime;

/**
 * Taken from LogicAJPlugin after refac
 * 
 * @author Mark Schmatz
 *
 */
public class CreateOutdirUtils
{
	private static final CreateOutdirUtils instance = new CreateOutdirUtils();
	
	public static CreateOutdirUtils getInstance()
	{
		return instance;
	}
	
	public IProject createOutputProject(IProject srcProject) throws CoreException
	{
		IProject destProject = null;
			String destProjectName = JTUtils.getOutputProjectName(srcProject);
			destProject = ResourcesPlugin.getWorkspace().getRoot().getProject(destProjectName);
			if ( !destProject.exists() )
			{
				destProject = createProject(destProjectName);
			}
			if ( !destProject.isOpen() )
			{
				destProject.open(null);
			}
			//IJavaProject destJavaProject = (IJavaProject) destProject.getNature(JavaCore.NATURE_ID);
			if (!destProject.hasNature(JavaCore.NATURE_ID))
			{
				JTUtils.copyAllNeededFiles(srcProject, destProject);
		        destProject.refreshLocal(IResource.DEPTH_INFINITE, null);
				addNature(destProject, JavaCore.NATURE_ID);
	
		        IClasspathEntry[] cp = ((JavaProject)srcProject.getNature(JavaCore.NATURE_ID)).getResolvedClasspath(true);
		        
		        for(int i=0;i<cp.length;i++){
		            if(cp[i].getEntryKind()==IClasspathEntry.CPE_SOURCE){
		            	if(cp[i].getPath().segmentCount() > 1 ) {
							IFolder folder = destProject.getFolder(cp[i].getPath().removeFirstSegments(1));
							if(!folder.exists()) {
								mkdirs(folder);
								//folder.create(true, true, null);
							}
		            	}
		            }
		        }
				//destJavaProject = (IJavaProject) destProject.getNature(JavaCore.NATURE_ID);
				//destJavaProject.setRawClasspath(cp, destProject.getFullPath(), null);
				// End - Schmatz
				IJavaProject javaProject = (IJavaProject) srcProject.getNature(JavaCore.NATURE_ID);
	
				IPath outPath = new Path("/"+destProject.getName());
	
				// temporary remove class path reference to the output project (if necessary)
				List filteredClassPath = JTUtils.getFilteredClasspath(outPath, javaProject);
	
				javaProject.setRawClasspath(
						(IClasspathEntry[])filteredClassPath.toArray(new IClasspathEntry[0]),
						null);
	
				IClasspathEntry[] newRaw = javaProject.getRawClasspath();
				// copyAllNeededFiles by Mark Schmatz
				JTUtils.copyAllNeededFiles(srcProject, destProject);
	
				if( destProject.exists() && !destProject.isOpen())
					destProject.open(null);
				
				// FIXME: schmatz: The next line consumes much time!!!
				JTransformerProjectNature.removeJTransformerNature(destProject);
	
				destProject.refreshLocal(IResource.DEPTH_INFINITE, null);
			}

		return destProject;
	}
	
    /**
     * @param dstFolder
     * @throws CoreException
     */
    private void mkdirs(IFolder dstFolder) throws CoreException {
        IContainer c = dstFolder;
        Stack v = new Stack();
        while (!c.exists()) {
            if (c.getType() != IResource.FOLDER) {
                throw new RuntimeException("Should not happen??!");
            }
            v.push(c);
            c = c.getParent();
        }
        while (!v.isEmpty()) {
            IFolder folder = (IFolder) v.pop();
            folder.create(true, true, null);
        }
    }



	/*
	 * Create simple project.
	 */
	private IProject createProject(final String projectName)
			throws CoreException
	{
		final IProject project = ResourcesPlugin.getWorkspace().getRoot()
				.getProject(projectName);
		IWorkspaceRunnable create = new IWorkspaceRunnable()
		{
			public void run(IProgressMonitor monitor) throws CoreException
			{
				if (project.exists())
				{
					try
					{
						project.open(null);
					} catch (Throwable t)
					{

					}
					project.delete(true, null);
				}
				project.create(null);
				project.open(null);
			}
		};
		ResourcesPlugin.getWorkspace().run(create, null);
		return project;
	}

	private void addNature(IProject project, String id) throws CoreException
	{
		if (!project.hasNature(id))
		{
			IProjectDescription ipd = project.getDescription();
			String[] oldNIDs = ipd.getNatureIds();
			String[] newNIDs = new String[oldNIDs.length + 1];
			newNIDs[0] = id;
			System.arraycopy(oldNIDs, 0, newNIDs, 1, oldNIDs.length);
			ipd.setNatureIds(newNIDs);
			if (!project.isSynchronized(IResource.DEPTH_ONE))
			{
				project.refreshLocal(IResource.DEPTH_ONE, null);
			}
			project.setDescription(ipd, null);
		}
	}
}

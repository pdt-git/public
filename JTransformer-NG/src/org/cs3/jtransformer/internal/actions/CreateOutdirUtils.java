package org.cs3.jtransformer.internal.actions;

import org.cs3.jtransformer.util.JTUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
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
		String destProjectName = JTUtils.getOutputProjectName(srcProject);
		IProject destProject = ResourcesPlugin.getWorkspace().getRoot().getProject(destProjectName);
		if ( !destProject.exists() )
		{
			destProject = createProject(destProjectName);
		}
		if ( !destProject.isOpen() )
		{
			destProject.open(null);
		}
		IJavaProject destJavaProject = (IJavaProject) destProject.getNature(JavaCore.NATURE_ID);
		if (!destProject.hasNature(JavaCore.NATURE_ID))
		{
			addNature(destProject, JavaCore.NATURE_ID);
			
			IClasspathEntry[] cp = new IClasspathEntry[] {
					JavaCore.newSourceEntry(destProject.getFullPath()),
					JavaRuntime.getDefaultJREContainerEntry(),

			};
			destJavaProject = (IJavaProject) destProject.getNature(JavaCore.NATURE_ID);
			destJavaProject.setRawClasspath(cp, destProject.getFullPath(), null);
		}
		return destProject;
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

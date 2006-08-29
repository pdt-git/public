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
	
	public IJavaProject createOutputProject() throws CoreException
	{
		String projectName = JTUtils.getOutputProjectName();
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(
				projectName);
		if (!project.exists())
		{
			project = createProject(projectName);
		}
		if (!project.isOpen())
		{
			project.open(null);
		}
		IJavaProject javaProject = (IJavaProject) project
				.getNature(JavaCore.NATURE_ID);
		if (!project.hasNature(JavaCore.NATURE_ID))
		{
			addNature(project, JavaCore.NATURE_ID);
			
			IClasspathEntry[] cp = new IClasspathEntry[] {
					JavaCore.newSourceEntry(project.getFullPath()),
					JavaRuntime.getDefaultJREContainerEntry(),

			};
			javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
			javaProject.setRawClasspath(cp, project.getFullPath(), null);
		}
		return javaProject;
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

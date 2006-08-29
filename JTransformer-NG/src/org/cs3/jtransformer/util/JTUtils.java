package org.cs3.jtransformer.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jdt.core.JavaModelException;

/**
 * Some util methods.
 * 
 * @author Mark Schmatz
 *
 */
public class JTUtils

{
	private static Boolean useSameOutdirWithSuffix = null;
	
	
	/**
	 * Returns true if the output project (the adapted version of the original)
	 * gets the same project name as the original project plus a suffix.<br>
	 * (this is set via a system property)
	 * 
	 * @return boolean
	 */
	public static boolean useSameOutdirWithSuffix()
	{
		if( useSameOutdirWithSuffix == null )
		{
			useSameOutdirWithSuffix = new Boolean(
				"true".equals(System.getProperty(JTConstants.SYSTEM_PROPERTY_USE_SAME_OUTDIR_WITH_SUFFIX, /*Default = */ "false")));
		}

		return useSameOutdirWithSuffix.booleanValue();
	}
	
	/**
	 * This method has dependencies to Eclipse.<br><br>
	 * 
	 * Returns the absolute path where the adapted version of the project
	 * is stored.<br><br>
	 * 
	 * If the the system property
	 * <tt>LAJConstants.SYSTEM_PROPERTY_USE_SAME_OUTDIR_WITH_SUFFIX</tt> is
	 * set to <tt>true</tt> then the 'original' project location plus a suffix
	 * is used.<br>
	 * Otherwise the default output project location is used
	 * (normally, ends with '<i>LogicAJOutput</i>').
	 * @param project 
	 * 
	 * @see JTConstants.SYSTEM_PROPERTY_USE_SAME_OUTDIR_WITH_SUFFIX
	 * 
	 * @return String the absolute path of the output dir
	 * @throws JavaModelException
	 */
	public static String getOutputProjectPath(IProject project)
	{
//		String outputProjectLocation = getOutputProjectLocation();
//		
//		if( outputProjectLocation == null )
//			System.err.println("************************ schmatz: outputProjectLocation is null");
//			
//		String outdir = null;
//		if( outputProjectLocation != null && JTUtils.useSameOutdirWithSuffix() )
//		{
//			outdir = outputProjectLocation + JTConstants.OUTPUT_PROJECT_NAME_SUFFIX;
//		}
//		else
//		{
//			/*
//			 * This is the original code:
//			 */
			return getWorkspaceRootLocation() + java.io.File.separator + JTUtils.getOutputProjectName(project);
//		}
//		return outdir;
	}

	/**
	 * Returns the output project name.
	 * If the the system property
	 * <tt>LAJConstants.SYSTEM_PROPERTY_USE_SAME_OUTDIR_WITH_SUFFIX</tt> is
	 * set to <tt>true</tt> then the project location of the 'original'
	 * project name plus a suffix.<br>
	 * Otherwise the default output project name is used
	 * (normally '<i>LogicAJOutput</i>').
	 *  
	 * @return String
	 */
	public static String getOutputProjectName(IProject project)
	{
		if( JTUtils.useSameOutdirWithSuffix() )
		{
			String outputProjectName = project.getName() + JTConstants.OUTPUT_PROJECT_NAME_SUFFIX;
			if( outputProjectName == null )
			{
				System.err.println("************************ schmatz: outputProjectName is null");
				outputProjectName = "LogicAJOutput";  // Fallback
			}
			return outputProjectName;
		}
		else
			return "LogicAJOutput";
	}

	// ------------------------------------------------------------
	
	/**
	 * Returns the location of the workspace root.
	 * 
	 * @return String The OS string
	 */
	public static String getWorkspaceRootLocation()
	{
		// Note: LogicAJPlugin.getDefault().getWorkspace() == ResourcesPlugin.getWorkspace() !!!
		if ( ResourcesPlugin.getWorkspace() != ResourcesPlugin.getWorkspace() )
		{
			System.err.println("************************** schmatz: Error in LAJUtil.getWorkspaceRootLocation()");
		}
		
		return ResourcesPlugin.getWorkspace().getRoot()
			.getLocation().toOSString();
	}
	
	/**
	 * Copies all needed files like the class path.
	 * 
	 * @param srcProject
	 * @param destProject
	 * @throws CoreException
	 */
	public static void copyAllNeededFiles(IProject srcProject, IProject destProject) throws CoreException
	{
		copyFile(srcProject, destProject, "/.classpath");
		copyFile(srcProject, destProject, "/.project");
		copyFile(srcProject, destProject, "/bundle.manifest");
		copyFile(srcProject, destProject, "/bundle-pack");
		
		// ---
		
		//adaptOutputProjectName(destProject, "/.project");
		//adaptOutputProjectName(destProject, "/bundle-pack");
	}

	private static void copyFile(IProject srcProject, IProject destProject, final String fileName) throws CoreException
	{
		IFile file = srcProject.getFile(new Path(fileName));
		if( file.exists() )
		{
			IFile old = destProject.getFile(new Path(fileName));
			old.refreshLocal(IResource.DEPTH_INFINITE, null);
			old.delete(true, true, null);
			file.copy(new Path(destProject.getFullPath() + fileName), true, null);
			destProject.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		}
	}
	
	private static void adaptOutputProjectName(IProject destProject, String fileName) throws CoreException
	{
		IFile file = destProject.getFile(new Path(fileName));
		if( file.exists() )
		{
		}
	}
}

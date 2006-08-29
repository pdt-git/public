package org.cs3.jtransformer.util;

import org.cs3.pdt.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
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
	 * 
	 * @see JTConstants.SYSTEM_PROPERTY_USE_SAME_OUTDIR_WITH_SUFFIX
	 * 
	 * @return String the absolute path of the output dir
	 * @throws JavaModelException
	 */
	public static String getOutputProjectPath()
	{
		String outputProjectLocation = getOutputProjectLocation();
		
		if( outputProjectLocation == null )
			System.err.println("************************ schmatz: outputProjectLocation is null");
			
		String outdir = null;
		if( outputProjectLocation != null && JTUtils.useSameOutdirWithSuffix() )
		{
			outdir = outputProjectLocation + JTConstants.OUTPUT_PROJECT_NAME_SUFFIX;
		}
		else
		{
			/*
			 * This is the original code:
			 */
			outdir = getWorkspaceRootLocation() + java.io.File.separator + JTUtils.getOutputProjectName();
		}
		return outdir;
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
	public static String getOutputProjectName()
	{
		if( JTUtils.useSameOutdirWithSuffix() )
		{
			String outputProjectName = getOutputProjectName_() + JTConstants.OUTPUT_PROJECT_NAME_SUFFIX;
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
	
	public static void copyClasspath(IProject srcProject, IProject destProject)
			throws CoreException
	{
		IFile old = destProject.getFile(new Path("/.classpath"));
		old.refreshLocal(IResource.DEPTH_INFINITE, null);
		old.delete(true, true, null);
		IFile file = srcProject.getFile(new Path("/.classpath"));
		file.copy(new Path(destProject.getFullPath() + "/.classpath"), true,
				null);
		destProject.refreshLocal(IResource.DEPTH_INFINITE,
				new NullProgressMonitor());
	}

	// ---------------------------------------------
	
	/**
	 * This method has dependencies to Eclipse.<br><br>
	 * 
	 * Returns the location (absolute path) of the
	 * current project.<br><br>
	 * 
	 * E.g. '<tt>C:/Dokumente und Einstellungen/Mark Schmatz/Eigene Dateien/runtime-EclipseApplication/MarksLogicAJDemo</tt>'
	 * 
	 * @param project
	 * @return String The absolute path of the given 's location
	 */
	private static String getOutputProjectLocation()
	{
		try
		{
			IFile f = UIUtils.getFileInActiveEditor();
			if( f != null )
			{
				String projectLocation = f.getProject().getLocation().makeAbsolute().toString();
				return projectLocation;
			}
		}
		catch (Exception e)
		{
			System.out.println("No active editor open");
		}
		
		return null;
	}
	
	private static String getOutputProjectName_()
	{
		String projectLocation = getOutputProjectLocation();
		if( projectLocation != null )
			return projectLocation.substring(projectLocation.lastIndexOf("/")+1);
		else
			return null;
	}
}

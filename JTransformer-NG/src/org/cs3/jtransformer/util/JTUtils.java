package org.cs3.jtransformer.util;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
	private static Boolean useSameProjectNameSuffix = null;
	
	
	/**
	 * Returns true if the output project (the adapted version of the original)
	 * gets the same project name as the original project plus a suffix.<br>
	 * (this is set via a system property)
	 * 
	 * @return boolean
	 */
	public static boolean useSameProjectNameSuffix()
	{
		if( useSameProjectNameSuffix == null )
		{
			useSameProjectNameSuffix = new Boolean(
				"true".equals(System.getProperty(JTConstants.SYSTEM_PROPERTY_USE_SAME_OUTDIR_WITH_SUFFIX, /*Default = */ "false")));
		}

		return useSameProjectNameSuffix.booleanValue();
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
		return getWorkspaceRootLocation() + java.io.File.separator + JTUtils.getOutputProjectName(project);
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
	 * @param srcProject The source project
	 * @return String
	 */
	public static String getOutputProjectName(IProject srcProject)
	{
		if( JTUtils.useSameProjectNameSuffix() )
		{
			String outputProjectName = srcProject.getName() + JTConstants.OUTPUT_PROJECT_NAME_SUFFIX;
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
	// New by Mark Schmatz
	public static void copyAllNeededFiles(IProject srcProject, IProject destProject) throws CoreException
	{
		if( !srcProject.isOpen() )
			srcProject.open(null);
		if( !destProject.isOpen() && destProject.exists() )
			destProject.open(null);
		
		if( destProject.isOpen() )
		{
			String srcProjectName = srcProject.getName();
			String destProjectName = destProject.getName();
			
			List neededFileForCopying = new ArrayList();
			neededFileForCopying.add(new CopyFileHelper("/.classpath"));
			neededFileForCopying.add(new CopyFileHelper("/bundle.manifest"));
			neededFileForCopying.add(new CopyFileHelper("/.project", srcProjectName, destProjectName));
			
			Map regexPatternsWithNewStrings = new HashMap();
			regexPatternsWithNewStrings.put(srcProjectName, destProjectName);
			regexPatternsWithNewStrings.put(
					"pattern=\"(.*?)\"",
					"pattern=\"" +
					"${CAPT_GROUP=1}" +
					"|.*" + File.separator + "\\.pl" +
					"|.*" + File.separator + "\\.aj" +
					"\""
			);
			neededFileForCopying.add(new CopyFileHelper("/.bundle-pack", regexPatternsWithNewStrings));
			
			Iterator iterator = neededFileForCopying.iterator();
			while( iterator.hasNext() )
			{
				CopyFileHelper cfh = (CopyFileHelper) iterator.next();
				copyFile(srcProject, destProject, cfh.getFileName());
				if( cfh.needsAdaptation() )
				{
					adaptOutputProjectName(destProject, cfh);
				}
			}
		}
	}

	/**
	 * Copies the file for <tt>fileName</tt> from the
	 * source project to the dest project if it exists.
	 * If it doesn't exist nothing will be done.
	 * 
	 * @param srcProject
	 * @param destProject
	 * @param fileName
	 * @throws CoreException
	 */
	// Modified by Mark Schmatz
	private static void copyFile(IProject srcProject, IProject destProject, final String fileName) throws CoreException
	{
		IFile file = srcProject.getFile(new Path(fileName));
		if( file.exists() )
		{
			IFile old = destProject.getFile(new Path(fileName));
			if( old.exists() )
			{
				// Just to be sure: delete the file if it exists...
				old.refreshLocal(IResource.DEPTH_INFINITE, null);
				old.delete(true, true, null);
			}
			file.copy(new Path(destProject.getFullPath() + fileName), true, null);
			destProject.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		}
	}
	
	/**
	 * 
	 * @param destProject
	 * @param fileName
	 * @throws CoreException
	 */
	// New by Mark Schmatz
	private static void adaptOutputProjectName(IProject destProject, CopyFileHelper cfh) throws CoreException
	{
		IFile file = destProject.getFile(new Path(cfh.getFileName()));
		if( file.exists() )
		{
			String fileContent = getFileContent(file);
			
			Map regexPatternsWithNewStrings = cfh.getRegexPatternsWithNewStrings();
			Iterator iterator = regexPatternsWithNewStrings.keySet().iterator();
			while( iterator.hasNext() )
			{
				String key = (String) iterator.next();
				String val = (String) regexPatternsWithNewStrings.get(key);
				
				Pattern pattern = Pattern.compile(key);
				Matcher matcher = pattern.matcher(fileContent);
				if( matcher.find() )
				{
					if( matcher.groupCount() > 0 )
					{
						for( int cpc=1 ; cpc <=matcher.groupCount() ; cpc++ )
						{
							String captGroup = matcher.group(cpc);
							captGroup = captGroup.replace("\\", "\\\\\\\\");
							val = val.replaceAll("\\$\\{CAPT_GROUP=" + cpc + "\\}", captGroup);
						}
					}
					fileContent = fileContent.replaceAll(key, val);
				}
			}
			
			byte[] buffer = fileContent.getBytes();
			InputStream is = new ByteArrayInputStream(buffer);
			
			file.setContents(is, IFile.FORCE, null);
		}
	}
	
	/**
	 * Returns the content of the file as String.
	 * 
	 * @param file
	 * @return String
	 * @throws CoreException
	 */
	private static String getFileContent(IFile file) throws CoreException
	{
		if( file.exists() )
		{
			try
			{
				StringBuffer strb = new StringBuffer();
				InputStream is = file.getContents();
				BufferedReader br = new BufferedReader(new InputStreamReader(is));
				String line = null;
				while( (line = br.readLine()) != null )
				{
					strb.append(line).append("\n");
				}
				br.close();
				is.close();
				
				return strb.toString();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}
		
		return null;
	}
}

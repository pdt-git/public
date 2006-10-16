package org.cs3.jtransformer.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.internal.natures.JTransformerProjectNature;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
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
	
	private static ThreadLocal tmpCTList = new ThreadLocal()
	{
		protected synchronized Object initialValue()
		{
			return new ArrayList();
		}
	};
	public static List getTmpCTList()
	{
		return (List) tmpCTList.get();
	}
	public static synchronized void setTmpCTList(List list)
	{
		getTmpCTList().clear();
		getTmpCTList().addAll(list);
	}

	
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
				"true".equals(System.getProperty(JTConstants.SYSTEM_PROPERTY_USE_SAME_OUTDIR_WITH_SUFFIX, /*Default = */ "true")));
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
		IProject outputProject= ResourcesPlugin.getWorkspace().getRoot().getProject(JTUtils.getOutputProjectName(project));
		if(outputProject.getLocation() == null) { // TODO: only necessary for the test framework
			return ResourcesPlugin.getWorkspace().getRoot()
			.getLocation().toPortableString() + "/" + JTUtils.getOutputProjectName(project); 
		}
		return outputProject.getLocation().toPortableString();

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
		boolean isBundle = false;
		
		if( !srcProject.isOpen() )
			srcProject.open(null);
		if( !destProject.isOpen() && destProject.exists() )
			destProject.open(null);
		
		if( destProject.isOpen() )
		{
			String srcProjectName = srcProject.getName();
			String destProjectName = destProject.getName();

			/*
			 * Check whether we have a OSGi bundle as source project...
			 */
			if( fileExists(srcProject, JTConstants.BUNDLE_MANIFEST_FILE) )
				isBundle = true;

			// ----
			
			List neededFileForCopying = new ArrayList();
			
			if( !isBundle )
			{
				neededFileForCopying.add(new FileAdaptationHelper(JTConstants.DOT_CLASSPATH_FILE));
			}
			neededFileForCopying.add(new FileAdaptationHelper(JTConstants.DOT_PROJECT_FILE, srcProjectName, destProjectName));

			/*
			 * Do the following only if we have a bundle
			 */
			if( isBundle )
			{
				{
					String pattern = "Export-Package:(.*)";
					String replaceString =
						"Export-Package: " +
						JTConstants.RESOURCES_FILELISTS_PACKAGE + ", " +
						//getCTPackagesAsCSV(getTmpCTList()) + ", " +
						"${CAPT_GROUP=1}";

					FileAdaptationHelper fah = adaptManifestFile(srcProject, destProject, pattern, replaceString);
					if( fah.getNotAdaptedPatterns().contains(pattern) )
					{
						/*
						 * The manifest has no 'Export-Package:' line => add it...
						 */

						pattern = "^(.*)$";
						replaceString =
							"${CAPT_GROUP=1}\n" +
							"Export-Package: " +
							JTConstants.RESOURCES_FILELISTS_PACKAGE + ", " +
							//getCTPackagesAsCSV(getTmpCTList()) +
							"\n\n";
						adaptManifestFile(srcProject, destProject, pattern, replaceString);
					}
				}	

				{
					Map regexPatternsWithNewStrings = new HashMap();
					regexPatternsWithNewStrings.put(srcProjectName, destProjectName);
					regexPatternsWithNewStrings.put(
							"pattern=\"(.*?)\"",
							"pattern=\"" +
							"${CAPT_GROUP=1}" +
							"|.*\\\\.pl" +
							"|.*\\\\.aj" +
							"|.*" + JTConstants.CT_LIST_FILENAME_WITHOUT_SUFFIX + "\\\\." + JTConstants.CT_LIST_SUFFIX +
							"|.*" + JTConstants.FQCN_LIST_FILENAME_WITHOUT_SUFFIX + "\\\\." + JTConstants.FQCN_LIST_SUFFIX +
							"\""
					);
					neededFileForCopying.add(new FileAdaptationHelper(JTConstants.BUNDLE_PACK_FILE, regexPatternsWithNewStrings));
				}	

				{
					Map regexPatternsWithNewStrings2 = new HashMap();
					regexPatternsWithNewStrings2.put(srcProjectName, destProjectName);
					regexPatternsWithNewStrings2.put(
							"\\<classpathentry\\s+?including=\"(.*?)\"",
							"<classpathentry including=\"" +
							"${CAPT_GROUP=1}" +
							"|**/" + JTConstants.CT_LIST_FILENAME + 
							"|**/" + JTConstants.FQCN_LIST_FILENAME + 
							"\""
					);
					neededFileForCopying.add(new FileAdaptationHelper(JTConstants.DOT_CLASSPATH_FILE, regexPatternsWithNewStrings2));
				}
			}
			
			// ----
			
			Iterator iterator = neededFileForCopying.iterator();
			while( iterator.hasNext() )
			{
				FileAdaptationHelper fah = (FileAdaptationHelper) iterator.next();
				copyFile(srcProject, destProject, fah.getFileName());
				if( fah.needsAdaptation() )
				{
					adaptFile(destProject, fah, false);
				}
			}
		}
	}

	private static FileAdaptationHelper adaptManifestFile(IProject srcProject, IProject destProject, String pattern, String replaceString) throws CoreException
	{
		Map regexPatternsWithNewStrings = new HashMap();
		regexPatternsWithNewStrings.put(
				pattern,
				replaceString
		);
		FileAdaptationHelper fah =
			new FileAdaptationHelper(JTConstants.BUNDLE_MANIFEST_FILE, regexPatternsWithNewStrings, JTConstants.RESOURCES_FILELISTS_PACKAGE);
		copyFile(srcProject, destProject, fah.getFileName());
		adaptFile(destProject, fah, true);
		return fah;
	}
	
	/**
	 * Returns <tt>true</tt> if the file for the given <tt>filename</tt> exists in
	 * <tt>srcProject</tt>; <tt>false</tt> otherwise.
	 * 
	 * @param srcProject
	 * @param filename
	 * @return boolean
	 */
	public static boolean fileExists(IProject srcProject, String filename)
	{
		IFile file = srcProject.getFile(new Path(filename));
		if( file.exists() )
			return true;
		else
			return false;
	}

	/**
	 * Stores the given CT names in the comma separated String <tt>ctNameList</tt>
	 * into a file in the given path (<tt>absolutePathOfOutputProject</tt>).
	 * 
	 * @param ctNameList
	 * @param absolutePathOfOutputProject
	 */
	// New by Mark Schmatz
	public static void storeCTList(String ctNameList, List ctFilenameList, String absolutePathOfOutputProject)
	{
		int i=0;
		
		List list = new ArrayList();
		StringTokenizer st = new StringTokenizer(ctNameList, ",");
		while( st.hasMoreTokens() )
		{
			String token = st.nextToken().trim();
			list.add(token + JTConstants.CTNAME_FILENAME_SEPARATOR + ctFilenameList.get(i++));
		}
		
		/*
		 * After the CT list is created and stored
		 * save it in a temp list so that after copying
		 * the manifest it can be extended by exporting
		 * the CT packages.
		 * 
		 */
		setTmpCTList(list);
		
		storeListInFile(list, absolutePathOfOutputProject, JTConstants.CT_LIST_FILENAME);
	}
	
	/**
	 * Returns the CT packages from the temp CT list
	 * as comma separated values String in order that
	 * it can be inserted in the manifest file.
	 *  
	 * @return Stirng
	 *
	public static String getCTPackagesAsCSV(List tmpCTList)
	{
		StringBuffer buffer = new StringBuffer();
		String str = "";
		
		if( tmpCTList != null )
		{
			Iterator iterator = tmpCTList.iterator();
			while( iterator.hasNext() )
			{
				String ctLine = (String) iterator.next();
				StringTokenizer st = new StringTokenizer(ctLine, JTConstants.CTNAME_FILENAME_SEPARATOR);
				String ctName = st.nextToken().trim().replaceAll("/", ".");
				String ctFilename = st.nextToken().trim();
				
				Pattern p = Pattern.compile("'(.*?)"+ctFilename+"'\\(.*?\\)");
				Matcher m = p.matcher(ctName);
				if( m.find() )
				{
					String ctPackage = m.group(1);
					// Delete trailing dot
					ctPackage = ctPackage.substring(0, ctPackage.length()-1);
					// Add the prefixing resources package
					String subDirForCts = makeItAPackagePart(JTConstants.SUBDIR_FOR_CTS);
					ctPackage = subDirForCts + ctPackage;

					if( buffer.indexOf(ctPackage) == -1 )
					{
						// We don't want duplicates...
						buffer.append(ctPackage).append(", ");
					}
				}
			}
			
			if( buffer.length() > 2)
				str = buffer.substring(0, buffer.length()-2);
		}
		
		return str;
	}

	public static String makeItAPackagePart(String subDirForCts)
	{
		if( JTConstants.SUBDIR_FOR_CTS != null && !"".equals(JTConstants.SUBDIR_FOR_CTS) )
			return JTConstants.SUBDIR_FOR_CTS + ".";
		else
			return "";
	}
	*/
	
	/**
	 * Stores the list of full qualified Java class names of all not-extern
	 * classes of the bundle (except the bundle activator) into a
	 * file in the given path (<tt>absolutePathOfOutputProject</tt>).
	 *
	 * @param prologSession
	 * @param absolutePathOfOutputProject
	 * @return <tt>true</tt> if everything went right; <tt>false</tt> otherwise
	 */
	// New by Mark Schmatz
	// TODO: schmatz: don't store the bundle activator
	public static boolean storeJavaFileList(PrologSession prologSession, String absolutePathOfOutputProject) throws PrologInterfaceException
	{
		boolean ok = true;

		// Note: fullQualifiedName/2 requires that at least one argument is bound!
		List queryList = prologSession.queryAll(
				"class(ResolvedServiceClassId,_,_),not(externT(ResolvedServiceClassId)), fullQualifiedName(ResolvedServiceClassId, FqClassName)."
		);
		
		if( queryList != null )
		{
			List list = new ArrayList();
			
			Iterator iterator = queryList.iterator();
			while( iterator.hasNext() )
			{
				HashMap map = (HashMap) iterator.next();
				String fqClassName = (String) map.get("FqClassName");
				list.add(fqClassName);
			}
			
			storeListInFile(list, absolutePathOfOutputProject, JTConstants.FQCN_LIST_FILENAME);
		}
		else
			ok = false;
	
		return ok;
	}

	/**
	 * Stores the Strings in the given list into the given file and path.
	 * 
	 * @param stringList
	 * @param absolutePath
	 */
	// New by Mark Schmatz
	public static void storeListInFile(List stringList, String absolutePath, String fileName)
	{
		try
		{
			BufferedWriter bw = new BufferedWriter(
					new FileWriter(getFileInstance(absolutePath + JTConstants.COMPLETE_RESOURCES_FILELISTS_FOLDER, fileName)));
			Iterator iterator = stringList.iterator();
			while( iterator.hasNext() )
			{
				String elem = (String) iterator.next();
				bw.write(elem + "\n");
			}
			bw.flush();
			bw.close();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * Returns a File instance for the given filename and path.
	 * If the path does not exist it will be deep-created.<br>
	 * The file may exist or not.
	 * 
	 * @param pathName
	 * @param filename
	 * @return File
	 */
	public static File getFileInstance(String pathName, String filename)
	{
		File path = new File(pathName);
		if( !path.exists() )
			path.mkdirs();
		
		if( !pathName.endsWith("/") && !pathName.endsWith("\\") )
			pathName += File.separator;
			
		File file = new File(pathName + filename);
		return file;
	}
	
	/**
	 * Util method returning the Prolog interface.
	 * 
	 * @param srcProject
	 * @return PrologInterface
	 * @throws CoreException
	 */
	public static PrologInterface getPrologInterface(IProject srcProject) throws CoreException
	{
		return ((JTransformerProjectNature) srcProject.getNature(JTransformer.NATURE_ID)).getPrologInterface();
	}
	
	// ------------------------------------------------------------------

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
			old.refreshLocal(IResource.DEPTH_INFINITE, null);
		}
	}

	/**
	 * Adapts the encapsulated file in the given helper so that it
	 * fits the needs in the output project.
	 *  
	 * @param destProject
	 * @param fileName
	 * @throws CoreException
	 */
	// New by Mark Schmatz
	private static void adaptFile(IProject destProject, FileAdaptationHelper cfh, boolean deleteInnerEmptyLines) throws CoreException
	{
		IFile file = destProject.getFile(new Path(cfh.getFileName()));
		if( file.exists() )
		{
			String fileContent = getFileContent(file);
			
			fileContent = cfh.adaptContent(fileContent);
		
			if( deleteInnerEmptyLines )
				fileContent = removeEmptyLines(fileContent) + "\n";
			
			byte[] buffer = fileContent.getBytes();
			InputStream is = new ByteArrayInputStream(buffer);
			
			file.setContents(is, IFile.FORCE, null);
		}
	}

	public static String removeEmptyLines(String str)
	{
		String tmp = "";
		while( !tmp.equals(str) )
		{
			tmp = str;
			str = str.replaceAll("\n\n", "\n");
			str = str.replaceAll("\r\n\r\n", "\r\n");
		}
		return str;
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
	/**
	 * Remove the output project from the class path.
	 * @param outPath
	 * @param javaProject
	 * @return
	 * @throws JavaModelException
	 */
	static public List getFilteredClasspath(IPath outPath, IJavaProject javaProject) throws JavaModelException {
		IClasspathEntry[] classpath = javaProject.getRawClasspath();
		
		List filteredClassPath = new ArrayList();
		for (int i = 0; i < classpath.length; i++) {
			IPath path= classpath[i].getPath();
			if(!path.equals(outPath)) {
				filteredClassPath.add(classpath[i]);
			}
		}
		return filteredClassPath;
	}
	/**
	 * 
	 * @param javaProject
	 * @param destProject
	 * @throws CoreException 
	 */
	public static void addReferenceToOutputProjectIfNecessary(IJavaProject javaProject, IProject destProject) throws CoreException {
		IPath outPath = new Path("/"+destProject.getName());
		List filteredClassPath = JTUtils.getFilteredClasspath(outPath, javaProject);
		// add reference to output project
		filteredClassPath.add(JavaCore.newProjectEntry(outPath));

		javaProject.setRawClasspath(
				(IClasspathEntry[])filteredClassPath.toArray(new IClasspathEntry[0]), null);

		javaProject.getProject().refreshLocal(IResource.DEPTH_INFINITE, null);		
	}
	
	/**
	 * 
	 * @param javaProject
	 * @param destProject
	 * @throws CoreException 
	 */
	public static void removeReferenceToOutputProjectIfNecessary(IJavaProject javaProject, IProject destProject, IProgressMonitor monitor) throws CoreException {
		IPath outPath = new Path("/"+destProject.getName());
		javaProject.setRawClasspath(
				(IClasspathEntry[])JTUtils.getFilteredClasspath(outPath, javaProject).toArray(new IClasspathEntry[0]), monitor);
		javaProject.getProject().refreshLocal(IResource.DEPTH_INFINITE, monitor);		
	}
	
	/**
	 * 
	 * @param lajProject
	 * @return
	 */
	public static IProject getOutputProject(IProject project) {
			return ResourcesPlugin.getWorkspace().getRoot().getProject(JTUtils.getOutputProjectName(project));
	}
	public static JTransformerProjectNature getNature(IProject project) throws CoreException {
		return (JTransformerProjectNature)project.getNature(JTransformer.NATURE_ID);
	}
}

/**
 * ld: most of the methds were copied from the eclipse test suites, so i include
 * the copyright notice below.
 */

/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.cs3.jlmp.tests;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.Map;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.jlmp.natures.JLMPProjectNature;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClassFile;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.ICodeAssist;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaModel;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IPackageDeclaration;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IParent;
import org.eclipse.jdt.core.IProblemRequestor;
import org.eclipse.jdt.core.ISourceReference;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.ToolFactory;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.core.compiler.IProblem;
import org.eclipse.jdt.core.formatter.CodeFormatter;
import org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants;
import org.eclipse.jdt.internal.core.util.Util;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.TextEdit;

public abstract class FactGenerationTest extends SuiteOfTestCases {

	private IJavaProject testJavaProject;

	private IProject testProject;

	private JLMPProjectNature testJLMPProject;

	private ResourceFileLocator testDataLocator;

	private DefaultResourceFileLocator testWorkspaceLocator;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jlmp.tests.SuiteOfTestCases#setUpOnce()
	 */
	public void setUpOnce() {
		super.setUpOnce();
		try {
			this.testJavaProject = createJavaProject("testproject");
			this.testProject = (IProject) testJavaProject.getResource();
			addNature(testProject,JLMP.NATURE_ID);
			this.testJLMPProject = (JLMPProjectNature) testProject
					.getNature(JLMP.NATURE_ID);

		} catch (CoreException e) {
			e.printStackTrace();
			fail();
		}
	}

	/**
	 * install testdata into the runtime workspace.
	 * 
	 * @param strings
	 *            pathnames relative to the testdata location. Directories will
	 *            be copied recursively.
	 */
	protected void install(String[] strings) {
		for (int i = 0; i < strings.length; i++) {
			String string = strings[i];
			install(string);
		}
	}

	/**
	 * @param string
	 */
	protected void install(String string) {
		File src = testDataLocator.resolve(string);
		File dst = testWorkspaceLocator.resolve(string); 
		
	}

	/**
	 * delete testdata from the runtime workspace.
	 * 
	 * @param strings
	 *            pathnames relative to the testdata location. Directories will
	 *            be copied recursively.
	 */
	protected void uninstall(String[] strings) {

	}
	/**
	 * @param data
	 */
	
	public FactGenerationTest(String name) {
		super(name);
		setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator("testdata"));
		IPath location = getWorkspaceRoot().getLocation();
		testWorkspaceLocator = new DefaultResourceFileLocator(new File(location.toOSString()));
		
		
	}

	
	protected void addNature(IProject project,String id) throws CoreException {
		if (!project.hasNature(id)) {
			IProjectDescription ipd = project.getDescription();
			String[] oldNIDs = ipd.getNatureIds();
			String[] newNIDs = new String[oldNIDs.length + 1];
			newNIDs[0] = id;
			System.arraycopy(oldNIDs, 0, newNIDs, 1, oldNIDs.length);
			ipd.setNatureIds(newNIDs);
			if (!project.isSynchronized(IResource.DEPTH_ONE)) {
				project.refreshLocal(IResource.DEPTH_ONE, null);
			}
			project.setDescription(ipd, null);
		}
	}

		
	protected void removeJLMPNature(IProject project,String id) throws CoreException {
		if (project.hasNature(id)) {
			IProjectDescription ipd = project.getDescription();
			String[] oldNIDs = ipd.getNatureIds();
			String[] newNIDs;
			newNIDs = new String[oldNIDs.length - 1];
			int j = 0;
			for (int i = 0; i < newNIDs.length; i++) {
				if (oldNIDs[j].equals(id))
					j++;
				newNIDs[i] = oldNIDs[j];
				j++;
			}
			ipd.setNatureIds(newNIDs);
			if (!project.isSynchronized(IResource.DEPTH_ONE)) {
				project.refreshLocal(IResource.DEPTH_ONE, null);
			}
			project.setDescription(ipd, null);
		}
	}

	

	

	/*
	 * Creates a Java project where prj=src=bin and with JCL_LIB on its
	 * classpath.
	 */
	protected IJavaProject createJavaProject(String projectName)
			throws CoreException {
		return this.createJavaProject(projectName, new String[] { "" },
				new String[] { "JCL_LIB" }, "");
	}

	/*
	 * Creates a Java project with the given source folders an output location.
	 * Add those on the project's classpath.
	 */
	protected IJavaProject createJavaProject(String projectName,
			String[] sourceFolders, String output) throws CoreException {
		return this.createJavaProject(projectName, sourceFolders,
				null/* no lib */, null/* no project */,
				null/* no exported project */, output,
				null/* no source outputs */, null/* no exclusion pattern */
		);
	}

	/*
	 * Creates a Java project with the given source folders an output location.
	 * Add those on the project's classpath.
	 */
	protected IJavaProject createJavaProject(String projectName,
			String[] sourceFolders, String output, String[] sourceOutputs)
			throws CoreException {
		return this.createJavaProject(projectName, sourceFolders,
				null/* no lib */, null/* no project */,
				null/* no exported project */, output, sourceOutputs, null/*
																		 * no
																		 * exclusion
																		 * pattern
																		 */
		);
	}

	protected IJavaProject createJavaProject(String projectName,
			String[] sourceFolders, String[] libraries, String output)
			throws CoreException {
		return this.createJavaProject(projectName, sourceFolders, libraries,
				null/* no project */, null/* no exported project */, output,
				null/* no source outputs */, null/* no exclusion pattern */
		);
	}

	protected IJavaProject createJavaProject(String projectName,
			String[] sourceFolders, String[] libraries, String[] projects,
			String projectOutput) throws CoreException {
		return this.createJavaProject(projectName, sourceFolders, libraries,
				projects, null/* no exported project */, projectOutput,
				null/* no source outputs */, null/* no exclusion pattern */
		);
	}

	protected IJavaProject createJavaProject(String projectName,
			String[] sourceFolders, String[] libraries, String[] projects,
			boolean[] exportedProject, String projectOutput)
			throws CoreException {
		return this.createJavaProject(projectName, sourceFolders, libraries,
				projects, exportedProject, projectOutput,
				null/* no source outputs */, null/* no exclusion pattern */
		);
	}

	protected IJavaProject createJavaProject(final String projectName,
			final String[] sourceFolders, final String[] libraries,
			final String[] projects, final boolean[] exportedProjects,
			final String projectOutput, final String[] sourceOutputs,
			final String[][] exclusionPatterns) throws CoreException {
		final IJavaProject[] result = new IJavaProject[1];
		IWorkspaceRunnable create = new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException {
				// create project
				IProject project2 = createProject(projectName);

				// set java nature
				addNature(project2,JavaCore.NATURE_ID);

				// create classpath entries
				IProject project = getWorkspaceRoot().getProject(projectName);
				IPath projectPath = project.getFullPath();
				int sourceLength = sourceFolders == null ? 0
						: sourceFolders.length;
				int libLength = libraries == null ? 0 : libraries.length;
				int projectLength = projects == null ? 0 : projects.length;
				IClasspathEntry[] entries = new IClasspathEntry[sourceLength
						+ libLength + projectLength];
				for (int i = 0; i < sourceLength; i++) {
					IPath sourcePath = new Path(sourceFolders[i]);
					int segmentCount = sourcePath.segmentCount();
					if (segmentCount > 0) {
						// create folder and its parents
						IContainer container = project;
						for (int j = 0; j < segmentCount; j++) {
							IFolder folder = container.getFolder(new Path(
									sourcePath.segment(j)));
							if (!folder.exists()) {
								folder.create(true, true, null);
							}
							container = folder;
						}
					}
					IPath outputPath = null;
					if (sourceOutputs != null) {
						// create out folder for source entry
						outputPath = sourceOutputs[i] == null ? null
								: new Path(sourceOutputs[i]);
						if (outputPath != null && outputPath.segmentCount() > 0) {
							IFolder output = project.getFolder(outputPath);
							if (!output.exists()) {
								output.create(true, true, null);
							}
						}
					}
					// exclusion patterns
					IPath[] exclusionPaths;
					if (exclusionPatterns == null) {
						exclusionPaths = new IPath[0];
					} else {
						String[] patterns = exclusionPatterns[i];
						int length = patterns.length;
						exclusionPaths = new IPath[length];
						for (int j = 0; j < length; j++) {
							String exclusionPattern = patterns[j];
							exclusionPaths[j] = new Path(exclusionPattern);
						}
					}
					// create source entry
					entries[i] = JavaCore.newSourceEntry(projectPath
							.append(sourcePath), exclusionPaths,
							outputPath == null ? null : projectPath
									.append(outputPath));
				}
				for (int i = 0; i < libLength; i++) {
					String lib = libraries[i];

					if (lib.equals(lib.toUpperCase())) { // all upper case is a
														 // var
						char[][] vars = CharOperation.splitOn(',', lib
								.toCharArray());
						entries[sourceLength + i] = JavaCore.newVariableEntry(
								new Path(new String(vars[0])),
								vars.length > 1 ? new Path(new String(vars[1]))
										: null, vars.length > 2 ? new Path(
										new String(vars[2])) : null);
					} else if (lib
							.startsWith("org.eclipse.jdt.core.tests.model.")) { // container
						entries[sourceLength + i] = JavaCore
								.newContainerEntry(new Path(lib));
					} else {
						IPath libPath = new Path(lib);
						if (!libPath.isAbsolute() && libPath.segmentCount() > 0
								&& libPath.getFileExtension() == null) {
							project.getFolder(libPath).create(true, true, null);
							libPath = projectPath.append(libPath);
						}
						entries[sourceLength + i] = JavaCore.newLibraryEntry(
								libPath, null, null);
					}
				}
				for (int i = 0; i < projectLength; i++) {
					boolean isExported = exportedProjects != null
							&& exportedProjects.length > i
							&& exportedProjects[i];
					entries[sourceLength + libLength + i] = JavaCore
							.newProjectEntry(new Path(projects[i]), isExported);
				}

				// create project's output folder
				IPath outputPath = new Path(projectOutput);
				if (outputPath.segmentCount() > 0) {
					IFolder output = project.getFolder(outputPath);
					if (!output.exists()) {
						output.create(true, true, null);
					}
				}

				// set classpath and output location
				IJavaProject javaProject = JavaCore.create(project);
				javaProject.setRawClasspath(entries, projectPath
						.append(outputPath), null);
				result[0] = javaProject;
			}
		};
		getWorkspace().run(create, null);
		return result[0];
	}

	/*
	 * Create simple project.
	 */
	protected IProject createProject(final String projectName)
			throws CoreException {
		final IProject project = getWorkspaceRoot().getProject(projectName);
		IWorkspaceRunnable create = new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException {
				project.create(null);
				project.open(null);
			}
		};
		getWorkspace().run(create, null);
		return project;
	}

	public void deleteFile(File file) {
		file = file.getAbsoluteFile();
		if (!file.exists())
			return;
		if (file.isDirectory()) {
			String[] files = file.list();
			//file.list() can return null
			if (files != null) {
				for (int i = 0; i < files.length; ++i) {
					deleteFile(new File(file, files[i]));
				}
			}
		}
		boolean success = file.delete();
		int retryCount = 60; // wait 1 minute at most
		while (!success && --retryCount >= 0) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
			}
			success = file.delete();
		}
		if (success)
			return;
		System.err.println("Failed to delete " + file.getPath());
	}

	protected void deleteProject(String projectName) throws CoreException {
		IProject project = this.getWorkspaceRoot().getProject(projectName);
		if (project.exists() && !project.isOpen()) { // force opening so that
													 // project can be deleted
													 // without logging (see bug
													 // 23629)
			project.open(null);
		}
		deleteResource(project);
	}

	/**
	 * Batch deletion of projects
	 */
	protected void deleteProjects(final String[] projectNames)
			throws CoreException {
		ResourcesPlugin.getWorkspace().run(new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException {
				if (projectNames != null) {
					for (int i = 0, max = projectNames.length; i < max; i++) {
						if (projectNames[i] != null)
							deleteProject(projectNames[i]);
					}
				}
			}
		}, null);
	}

	/**
	 * Delete this resource.
	 */
	public void deleteResource(IResource resource) throws CoreException {
		CoreException lastException = null;
		try {
			resource.delete(true, null);
		} catch (CoreException e) {
			lastException = e;
		}
		int retryCount = 60; // wait 1 minute at most
		while (resource.isAccessible() && --retryCount >= 0) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
			}
			try {
				resource.delete(true, null);
			} catch (CoreException e) {
				lastException = e;
			}
		}
		if (!resource.isAccessible())
			return;
		System.err.println("Failed to delete " + resource.getFullPath());
		if (lastException != null) {
			throw lastException;
		}
	}

	/**
	 * Ensure that the positioned element is in the correct position within the
	 * parent.
	 */
	public void ensureCorrectPositioning(IParent container,
			IJavaElement sibling, IJavaElement positioned)
			throws JavaModelException {
		IJavaElement[] children = container.getChildren();
		if (sibling != null) {
			// find the sibling
			boolean found = false;
			for (int i = 0; i < children.length; i++) {
				if (children[i].equals(sibling)) {
					assertTrue("element should be before sibling", i > 0
							&& children[i - 1].equals(positioned));
					found = true;
					break;
				}
			}
			assertTrue("Did not find sibling", found);
		}
	}

	/**
	 * Returns the specified compilation unit in the given project, root, and
	 * package fragment or <code>null</code> if it does not exist.
	 */
	public IClassFile getClassFile(String projectName, String rootPath,
			String packageName, String className) throws JavaModelException {
		IPackageFragment pkg = getPackageFragment(projectName, rootPath,
				packageName);
		if (pkg == null) {
			return null;
		} else {
			return pkg.getClassFile(className);
		}
	}

	protected ICompilationUnit getCompilationUnit(String path) {
		return (ICompilationUnit) JavaCore.create(getFile(path));
	}

	/**
	 * Returns the specified compilation unit in the given project, root, and
	 * package fragment or <code>null</code> if it does not exist.
	 */
	public ICompilationUnit getCompilationUnit(String projectName,
			String rootPath, String packageName, String cuName)
			throws JavaModelException {
		IPackageFragment pkg = getPackageFragment(projectName, rootPath,
				packageName);
		if (pkg == null) {
			return null;
		} else {
			return pkg.getCompilationUnit(cuName);
		}
	}

	/**
	 * Returns the specified compilation unit in the given project, root, and
	 * package fragment or <code>null</code> if it does not exist.
	 */
	public ICompilationUnit[] getCompilationUnits(String projectName,
			String rootPath, String packageName) throws JavaModelException {
		IPackageFragment pkg = getPackageFragment(projectName, rootPath,
				packageName);
		if (pkg == null) {
			return null;
		} else {
			return pkg.getCompilationUnits();
		}
	}

	protected ICompilationUnit getCompilationUnitFor(IJavaElement element) {

		if (element instanceof ICompilationUnit) {
			return (ICompilationUnit) element;
		}

		if (element instanceof IMember) {
			return ((IMember) element).getCompilationUnit();
		}

		if (element instanceof IPackageDeclaration
				|| element instanceof IImportDeclaration) {
			return (ICompilationUnit) element.getParent();
		}

		return null;

	}

	protected IFile getFile(IPath path) {
		return getWorkspaceRoot().getFile(path);
	}

	protected IFile getFile(String path) {
		return getFile(new Path(path));
	}

	/**
	 * Returns the Java Model this test suite is running on.
	 */
	public IJavaModel getJavaModel() {
		return JavaCore.create(getWorkspaceRoot());
	}

	/**
	 * Returns the Java Project with the given name in this test suite's model.
	 * This is a convenience method.
	 */
	public IJavaProject getJavaProject(String name) {
		IProject project = getWorkspaceRoot().getProject(name);
		return JavaCore.create(project);
	}

	

	/**
	 * Returns the specified package fragment in the given project and root, or
	 * <code>null</code> if it does not exist. The rootPath must be specified
	 * as a project relative path. The empty path refers to the default package
	 * fragment.
	 */
	public IPackageFragment getPackageFragment(String projectName,
			String rootPath, String packageName) throws JavaModelException {
		IPackageFragmentRoot root = getPackageFragmentRoot(projectName,
				rootPath);
		if (root == null) {
			return null;
		} else {
			return root.getPackageFragment(packageName);
		}
	}

	/**
	 * Returns the specified package fragment root in the given project, or
	 * <code>null</code> if it does not exist. If relative, the rootPath must
	 * be specified as a project relative path. The empty path refers to the
	 * package fragment root that is the project folder iteslf. If absolute, the
	 * rootPath refers to either an external jar, or a resource internal to the
	 * workspace
	 */
	public IPackageFragmentRoot getPackageFragmentRoot(String projectName,
			String rootPath) throws JavaModelException {

		IJavaProject project = getJavaProject(projectName);
		if (project == null) {
			return null;
		}
		IPath path = new Path(rootPath);
		if (path.isAbsolute()) {
			IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace()
					.getRoot();
			IResource resource = workspaceRoot.findMember(path);
			IPackageFragmentRoot root;
			if (resource == null) {
				// external jar
				root = project.getPackageFragmentRoot(rootPath);
			} else {
				// resource in the workspace
				root = project.getPackageFragmentRoot(resource);
			}
			if (root.exists()) {
				return root;
			}
		} else {
			IPackageFragmentRoot[] roots = project.getPackageFragmentRoots();
			if (roots == null || roots.length == 0) {
				return null;
			}
			for (int i = 0; i < roots.length; i++) {
				IPackageFragmentRoot root = roots[i];
				if (!root.isExternal()
						&& root.getUnderlyingResource()
								.getProjectRelativePath().equals(path)) {
					return root;
				}
			}
		}
		return null;
	}

	/**
	 * Returns the IWorkspace this test suite is running on.
	 * 
	 * @deprecated
	 */
	public IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	/**
	 * @deprecated
	 * @return
	 */
	public IWorkspaceRoot getWorkspaceRoot() {
		return getWorkspace().getRoot();
	}

	public byte[] read(java.io.File file) throws java.io.IOException {
		int fileLength;
		byte[] fileBytes = new byte[fileLength = (int) file.length()];
		java.io.FileInputStream stream = new java.io.FileInputStream(file);
		int bytesRead = 0;
		int lastReadSize = 0;
		while ((lastReadSize != -1) && (bytesRead != fileLength)) {
			lastReadSize = stream.read(fileBytes, bytesRead, fileLength
					- bytesRead);
			bytesRead += lastReadSize;
		}
		stream.close();
		return fileBytes;
	}

	public String read(IFile file) throws CoreException, IOException {
		if (!file.isSynchronized(IResource.DEPTH_ZERO))
			file.refreshLocal(IResource.DEPTH_ZERO, null);
		StringBuffer sb = new StringBuffer();
		BufferedReader reader = new BufferedReader(new InputStreamReader(file
				.getContents()));
		String line = reader.readLine();
		while (line != null) {
			sb.append(line);
			line = reader.readLine();
			if (line != null) {
				sb.append(System.getProperty("line.separator"));
			}
		}
		reader.close();
		return sb.toString();
	}

	protected void removeJavaNature(String projectName) throws CoreException {
		IProject project = this.getWorkspaceRoot().getProject(projectName);
		IProjectDescription description = project.getDescription();
		description.setNatureIds(new String[] {});
		project.setDescription(description, null);
	}

	/**
	 * Sets the class path of the Java project.
	 */
	public void setClasspath(IJavaProject javaProject,
			IClasspathEntry[] classpath) {
		try {
			javaProject.setRawClasspath(classpath, null);
		} catch (JavaModelException e) {
			assertTrue("failed to set classpath", false);
		}
	}

	protected void sortElements(IJavaElement[] elements) {
		Util.Comparer comparer = new Util.Comparer() {
			public int compare(Object a, Object b) {
				IJavaElement elementA = (IJavaElement) a;
				IJavaElement elementB = (IJavaElement) b;
				return elementA.getElementName().compareTo(
						elementB.getElementName());
			}
		};
		Util.sort(elements, comparer);
	}

	protected void sortResources(Object[] resources) {
		Util.Comparer comparer = new Util.Comparer() {
			public int compare(Object a, Object b) {
				IResource resourceA = (IResource) a;
				IResource resourceB = (IResource) b;
				return resourceA.getName().compareTo(resourceB.getName());
			}
		};
		Util.sort(resources, comparer);
	}

	protected void sortTypes(IType[] types) {
		Util.Comparer comparer = new Util.Comparer() {
			public int compare(Object a, Object b) {
				IType typeA = (IType) a;
				IType typeB = (IType) b;
				return typeA.getFullyQualifiedName().compareTo(
						typeB.getFullyQualifiedName());
			}
		};
		Util.sort(types, comparer);
	}

	/**
	 * convenience method.
	 * 
	 * @see JLMPProjectNature.writeFacts()
	 * @param icu
	 * @param outFile
	 * @return
	 * @throws IOException
	 * @throws CoreException
	 */
	protected IFile writeFactsToFile(ICompilationUnit icu) throws IOException,
			CoreException {
		IFile file = (IFile) icu.getResource();
		IPath inPath = file.getFullPath();
		IPath outPath = inPath.removeFileExtension().addFileExtension(
				"actual.pl");
		return writeFactsToFile(icu, getWorkspaceRoot().getFile(outPath));
	}

	/**
	 * convenience method.
	 * 
	 * @see JLMPProjectNature.writeFacts()
	 * @param icu
	 * @param outFile
	 * @return
	 * @throws IOException
	 * @throws CoreException
	 */
	protected IFile writeFactsToFile(ICompilationUnit icu, IFile outFile)
			throws IOException, CoreException {
		//ensureAccessible(outFile);
		IFile file = (IFile) icu.getResource();
		icu.becomeWorkingCopy(new IProblemRequestor() {
			public void acceptProblem(IProblem problem) {
				assertTrue(problem.getMessage(), problem.isWarning());
			}

			public void beginReporting() {
			}

			public void endReporting() {
			}

			public boolean isActive() {
				return true;
			}
		}, null);

		IPath inPath = file.getFullPath();
		IPath outPath = outFile.getFullPath();
		String outPathFsString = outFile.getRawLocation().toFile()
				.getAbsolutePath();
		PrintStream out = new PrintStream(new BufferedOutputStream(
				new FileOutputStream(outPathFsString)));
		getTestJLMPProject().writeFacts(icu, out);
		out.close();
		return outFile;
	}

	/**
	 * change the formating of the cu into an normalized form. the operation is
	 * performed in-place.
	 * 
	 * @param cu
	 * @throws JavaModelException
	 * @throws BadLocationException
	 *  
	 */
	protected void normalizeCompilationUnit(ICompilationUnit cu)
			throws JavaModelException, BadLocationException {
		String orig = cu.getSource();

		Map options = DefaultCodeFormatterConstants
				.getJavaConventionsSettings();

		//use the code formatter:

		//ld: Actualy not correct, but a quick and effective way of solving our
		// problem
		//FIXME better disable line splitting entirely, once the jdt api
		// stabelized at this point.
		options
				.put(DefaultCodeFormatterConstants.FORMATTER_LINE_SPLIT,
						"99999");
		options
				.put(
						DefaultCodeFormatterConstants.FORMATTER_NUMBER_OF_EMPTY_LINES_TO_PRESERVE,
						"0");

		CodeFormatter formatter = ToolFactory.createCodeFormatter(options);
		TextEdit te = formatter.format(CodeFormatter.K_COMPILATION_UNIT, orig,
				0, orig.length(), 0, System.getProperty("line.separator"));
		IDocument doc = new Document(orig);
		te.apply(doc);
		String formatted = doc.get();

		//remove block comments:
		formatted = formatted.replaceAll("(?s)/\\*.*\\*/", "");
		//remove line comments
		formatted = formatted.replaceAll("(?m)^\\s*//.*", "");
		//remove empty lines
		formatted = formatted.replaceAll("(?s)\\n\\s*\\n", "\n");
		//save changes.
		cu.getBuffer().replace(0, orig.length(), formatted);
		cu.save(null, true);
	}

	/**
	 * create a file with specified content within the runtime workspace.
	 * 
	 * @param path
	 *            workspace relative path. Uses forward slashes ('/') as a file
	 *            separator,
	 * @param contents
	 *            the contents of the file.
	 * @return the created IFile
	 * @throws CoreException
	 */
	protected IFile createFile(final String path, final String contents)
			throws CoreException {
		final Reader reader = new StringReader(contents);
		InputStream input = new InputStream() {
			public int read() throws IOException {
				return reader.read();
			}
		};
		IFile file = getWorkspaceRoot().getFile(new Path(path));
		file.create(input, true, null);
		return file;
	}

	/**
	 * enable/disable auto building workspace-wide.
	 * 
	 * @param value
	 */
	protected void setAutoBuilding(boolean value) {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceDescription desc = workspace.getDescription();
		desc.setAutoBuilding(value);

		try {
			workspace.setDescription(desc);
		} catch (Exception ex) {
			System.err.println(ex);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jlmp.tests.SuiteOfTestCases#getKey()
	 */
	protected Object getKey() {
		return this.getClass();
	}

	public IJavaProject getTestJavaProject() {
		return testJavaProject;
	}
	

	public JLMPProjectNature getTestJLMPProject() {
		return testJLMPProject;
	}

	public IProject getTestProject() {
		return testProject;
	}
	public ResourceFileLocator getTestDataLocator() {
		return testDataLocator;
	}
	public void setTestDataLocator(ResourceFileLocator testDataLocator) {
		this.testDataLocator = testDataLocator;
	}
}

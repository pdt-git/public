/**
 * ld: some  of the methds were copied from the eclipse test suites, so i include
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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import java.util.Vector;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.jlmp.builders.FactBaseBuilder;
import org.cs3.jlmp.builders.JLMPProjectBuilder;
import org.cs3.jlmp.natures.JLMPProjectNature;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
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
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IPackageDeclaration;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IParent;
import org.eclipse.jdt.core.IProblemRequestor;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.ToolFactory;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.core.compiler.IProblem;
import org.eclipse.jdt.core.formatter.CodeFormatter;
import org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants;
import org.eclipse.jdt.internal.core.util.Util;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.TextEdit;

/**
 * sets up a test project and offers some convenience methods. This class can be
 * used as super class for fact generation tests. It creates a project named
 * "testproject" in the runtime workspace and offeres methods to install test
 * files into this project.
 * 
 * <p>
 * Right now the test project has a single package fragment root which is the
 * project folder itself. The setupOnce method will be used to create the
 * project and add jdt and jlmp natures to it.
 */
public abstract class FactGenerationTest extends SuiteOfTestCases {

    private IJavaProject testJavaProject;

    private IProject testProject;

    private JLMPProjectNature testJLMPProject;

    private ResourceFileLocator testDataLocator;

    private DefaultResourceFileLocator testWorkspaceLocator;

    private static Object pifLock = new Object();

    public void waitForPif() throws InterruptedException {
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        while(!pif.isUp()){
            synchronized(pifLock){
                pifLock.wait();
            }
        }
    }

    /**
     * Wait for autobuild notification to occur
     */
    public static void waitForAutoBuild() {
        boolean wasInterrupted = false;
        do {
            try {
                Platform.getJobManager().join(
                        ResourcesPlugin.FAMILY_AUTO_BUILD, null);
                wasInterrupted = false;
            } catch (OperationCanceledException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                wasInterrupted = true;
            }
        } while (wasInterrupted);
    }

    /**
     * Wait for autobuild notification to occur
     */
    public static void waitForManualBuild() {
        boolean wasInterrupted = false;
        do {
            try {
                Platform.getJobManager().join(
                        ResourcesPlugin.FAMILY_MANUAL_BUILD, null);
                wasInterrupted = false;
            } catch (OperationCanceledException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                wasInterrupted = true;
            }
        } while (wasInterrupted);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.tests.SuiteOfTestCases#setUpOnce()
     */
    public void setUpOnce() {
        super.setUpOnce();
        try {
            IProject project = createProject("testproject");
            project.open(null);
            addNature(project, JavaCore.NATURE_ID);
            addNature(project, JLMP.NATURE_ID);
            IClasspathEntry[] cp = new IClasspathEntry[] {
                    JavaCore.newSourceEntry(project.getFullPath()),
                    JavaRuntime.getDefaultJREContainerEntry(),

            };
            getTestJavaProject().setRawClasspath(cp, project.getFullPath(),
                    null);
            PrologInterface pif = getTestJLMPProject().getPrologInterface();
            pif.addLifeCycleHook(new LifeCycleHook() {
                public void onInit(PrologSession initSession) {
                }

                public void afterInit() {
                    synchronized (pifLock) {
                        pifLock.notifyAll();
                    }
                }

                public void beforeShutdown(PrologSession session) {
                }
            });
        } catch (CoreException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * install testdata into the runtime workspace.
     * 
     * @param strings
     *                  pathnames relative to the testdata location. Directories will
     *                  be copied recursively.
     * @throws IOException
     * @throws CoreException
     */
    protected void install(String[] strings) throws CoreException, IOException {
        for (int i = 0; i < strings.length; i++) {
            String string = strings[i];
            install(string);
        }
    }

    /**
     * @param string
     * @throws CoreException
     * @throws IOException
     */
    protected void install(String string) throws CoreException, IOException {
        File src = testDataLocator.resolve(string);
        if (!src.exists()) {
            throw new FileNotFoundException(src.toString());
        }

        IProject project = getTestProject();
        IResource dst = project.findMember(string);
        if (dst != null && dst.exists()) {
            dst.delete(true, null);
        }
        try {
            if (src.isDirectory()) {
                IFolder dstFolder = project.getFolder(new Path(string));
                mkdirs(dstFolder);
                dstFolder.refreshLocal(IResource.DEPTH_ZERO, null);
                String[] strings = src.list();
                for (int i = 0; i < strings.length; i++) {
                    install(string + "/" + strings[i]);
                }
            } else {
                IFile dstFile = project.getFile(new Path(string));
                BufferedInputStream in = new BufferedInputStream(
                        new FileInputStream(src));
                dstFile.create(in, true, null);
                dstFile.refreshLocal(IResource.DEPTH_ZERO, null);
            }

        } catch (CoreException e) {
            throw new RuntimeException(e);
        }
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

    /**
     * delete testdata from the runtime workspace.
     * 
     * @param strings
     *                  pathnames relative to the testdata location. Directories will
     *                  uninstalled recursively.
     * @throws CoreException
     */
    protected void uninstall(String[] strings) throws CoreException {
        for (int i = 0; i < strings.length; i++) {
            String string = strings[i];
            uninstall(string);
        }
    }

    /**
     * @param string
     * @throws CoreException
     */
    protected void uninstall(String string) throws CoreException {
        IResource r = getTestProject().findMember(string);
        r.delete(true, null);
    }

    /**
     * @param data
     */

    public FactGenerationTest(String name) {
        super(name);
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator(
                "testdata"));
        IPath location = ResourcesPlugin.getWorkspace().getRoot().getLocation();
        testWorkspaceLocator = new DefaultResourceFileLocator(new File(location
                .toOSString()));

    }

    protected void addNature(IProject project, String id) throws CoreException {
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

    protected void removeNature(IProject project, String id)
            throws CoreException {
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
    private IJavaProject createJavaProject(String projectName)
            throws CoreException {
        return this.createJavaProject(projectName, new String[] { "" },
                new String[] { "JCL_LIB" }, "");
    }

    /*
     * Creates a Java project with the given source folders an output location.
     * Add those on the project's classpath.
     */
    private IJavaProject createJavaProject(String projectName,
            String[] sourceFolders, String output) throws CoreException {
        return this.createJavaProject(projectName, sourceFolders,
                null/* no lib */, null/* no project */,
                null/* no exported project */, output,
                null/* no source outputs */, null/*
                                                             * no exclusion
                                                             * pattern
                                                             */
        );
    }

    /*
     * Creates a Java project with the given source folders an output location.
     * Add those on the project's classpath.
     */
    private IJavaProject createJavaProject(String projectName,
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

    private IJavaProject createJavaProject(String projectName,
            String[] sourceFolders, String[] libraries, String output)
            throws CoreException {
        return this.createJavaProject(projectName, sourceFolders, libraries,
                null/* no project */, null/* no exported project */, output,
                null/* no source outputs */, null/*
                                                             * no exclusion
                                                             * pattern
                                                             */
        );
    }

    private IJavaProject createJavaProject(String projectName,
            String[] sourceFolders, String[] libraries, String[] projects,
            String projectOutput) throws CoreException {
        return this.createJavaProject(projectName, sourceFolders, libraries,
                projects, null/* no exported project */, projectOutput,
                null/* no source outputs */, null/*
                                                             * no exclusion
                                                             * pattern
                                                             */
        );
    }

    private IJavaProject createJavaProject(String projectName,
            String[] sourceFolders, String[] libraries, String[] projects,
            boolean[] exportedProject, String projectOutput)
            throws CoreException {
        return this.createJavaProject(projectName, sourceFolders, libraries,
                projects, exportedProject, projectOutput,
                null/* no source outputs */, null/*
                                                             * no exclusion
                                                             * pattern
                                                             */
        );
    }

    private IJavaProject createJavaProject(final String projectName,
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
                addNature(project2, JavaCore.NATURE_ID);

                // create classpath entries
                IProject project = ResourcesPlugin.getWorkspace().getRoot()
                        .getProject(projectName);
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

                    if (lib.equals(lib.toUpperCase())) { // all upper
                        // case is a
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
        ResourcesPlugin.getWorkspace().run(create, null);
        return result[0];
    }

    /*
     * Create simple project.
     */
    private IProject createProject(final String projectName)
            throws CoreException {
        final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                .getProject(projectName);
        IWorkspaceRunnable create = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
                if (project.exists()) {
                    try {
                        project.open(null);
                    } catch (Throwable t) {

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
        FactGenerationTest r = this;
        IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(
                projectName);
        if (project.exists() && !project.isOpen()) { // force
            // opening so
            // that
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

    public ICompilationUnit getCompilationUnit(String packageName, String cuName)
            throws JavaModelException {
        return getCompilationUnit("", packageName, cuName);
    }

    /**
     * Returns the specified compilation unit in the given project, root, and
     * package fragment or <code>null</code> if it does not exist.
     */
    private ICompilationUnit getCompilationUnit(String rootPath,
            String packageName, String cuName) throws JavaModelException {
        IPackageFragment pkg = getPackageFragment(rootPath, packageName);
        if (pkg == null) {
            return null;
        } else {
            return pkg.getCompilationUnit(cuName);
        }
    }

    public ICompilationUnit[] getCompilationUnitsInFolder(String packageName)
            throws CoreException {
        IFolder folder = getTestProject().getFolder(packageName);
        folder.refreshLocal(IResource.DEPTH_INFINITE, null);
        IResource[] resources = folder.members();
        Vector l = new Vector();
        for (int i = 0; i < resources.length; i++) {
            IResource r = resources[i];
            if (r.getType() == IResource.FILE
                    && "java".equals(r.getFileExtension())) {
                ICompilationUnit icu = (ICompilationUnit) JavaCore.create(r);
                assertFalse(icu.isWorkingCopy());
                l.add(icu);
            }
        }
        return (ICompilationUnit[]) l.toArray(new ICompilationUnit[0]);

    }

    /**
     * Returns the specified compilation unit in the given project, root, and
     * package fragment or <code>null</code> if it does not exist.
     */
    private ICompilationUnit[] getCompilationUnits(String rootPath,
            String packageName) throws JavaModelException {
        IPackageFragment pkg = getPackageFragment(rootPath, packageName);

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

    public IPackageFragment getPackageFragment(String packageName)
            throws JavaModelException {
        return getPackageFragment("", packageName);
    }

    /**
     * Returns the specified package fragment in the given project and root, or
     * <code>null</code> if it does not exist. The rootPath must be specified
     * as a project relative path. The empty path refers to the default package
     * fragment.
     */
    private IPackageFragment getPackageFragment(String rootPath,
            String packageName) throws JavaModelException {
        IPackageFragmentRoot root = getPackageFragmentRoot(rootPath);
        if (root == null) {
            return null;
        } else {
            return root.getPackageFragment(packageName);
        }
    }

    public IPackageFragmentRoot getPackageFragmentRoot()
            throws JavaModelException {
        return getPackageFragmentRoot("");
    }

    /**
     * Returns the specified package fragment root in the given project, or
     * <code>null</code> if it does not exist. If relative, the rootPath must
     * be specified as a project relative path. The empty path refers to the
     * package fragment root that is the project folder iteslf. If absolute, the
     * rootPath refers to either an external jar, or a resource internal to the
     * workspace
     */
    private IPackageFragmentRoot getPackageFragmentRoot(String rootPath)
            throws JavaModelException {

        IJavaProject project = getTestJavaProject();
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
        return writeFactsToFile(icu, ResourcesPlugin.getWorkspace().getRoot()
                .getFile(outPath));
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
        getTestJLMPProject().getFactBaseBuilder().writeFacts(icu, out);
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
        assertFalse(cu.isWorkingCopy());
        cu.becomeWorkingCopy(null, null);
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
        cu.commitWorkingCopy(false, null);

        cu.save(null, true);
        cu.discardWorkingCopy();
        assertFalse(cu.isWorkingCopy());
    }

    /**
     * create a file with specified content within the runtime workspace.
     * 
     * @param path
     *                  workspace relative path. Uses forward slashes ('/') as a file
     *                  separator,
     * @param contents
     *                  the contents of the file.
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
        IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(
                new Path(path));
        file.create(input, true, null);
        assertTrue(file.isAccessible());
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
        if (testJavaProject == null) {
            try {
                testJavaProject = (IJavaProject) getTestProject().getNature(
                        JavaCore.NATURE_ID);
            } catch (CoreException e) {
                throw new RuntimeException(e);
            }
        }
        return testJavaProject;
    }

    public JLMPProjectNature getTestJLMPProject() {
        if (testJLMPProject == null) {
            try {
                testJLMPProject = (JLMPProjectNature) getTestProject()
                        .getNature(JLMP.NATURE_ID);
            } catch (CoreException e) {
                throw new RuntimeException(e);
            }
        }
        return testJLMPProject;
    }

    public IProject getTestProject() {
        if (testProject == null) {
            testProject = ResourcesPlugin.getWorkspace().getRoot().getProject(
                    "testproject");
        }
        return testProject;
    }

    public ResourceFileLocator getTestDataLocator() {
        return testDataLocator;
    }

    public void setTestDataLocator(ResourceFileLocator testDataLocator) {
        this.testDataLocator = testDataLocator;
    }

    class _ProgressMonitor extends NullProgressMonitor {
        public Object lock = new Object();

        public boolean done = false;

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.NullProgressMonitor#done()
         */
        public void done() {
            done = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }

        public void waitTillDone() {
            while (!done) {
                synchronized (lock) {
                    try {
                        lock.wait();
                    } catch (InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
    };

    protected void build(String builder) throws CoreException {
        build(builder, new HashMap());
    }

    protected void build(String builder, Map args) throws CoreException {
        IProject project = getTestProject();
        _ProgressMonitor m = new _ProgressMonitor();
        project.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, builder,
                args, m);
        m.waitTillDone();
    }

    protected void build() throws CoreException {
        IProject project = getTestProject();
        _ProgressMonitor m = new _ProgressMonitor();
        project.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, m);
        m.waitTillDone();
    }
}

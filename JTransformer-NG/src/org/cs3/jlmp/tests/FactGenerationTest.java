package org.cs3.jlmp.tests;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Vector;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.jlmp.natures.JLMPProjectNature;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.internal.resources.ResourceException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
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
import org.eclipse.jdt.core.IBuffer;
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
import org.eclipse.jdt.core.WorkingCopyOwner;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.core.compiler.IProblem;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
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

    //private static Object pifLock = new Object();

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
    public void setUpOnce() throws Exception {
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

        } catch (CoreException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * install testdata into the runtime workspace.
     * 
     * @param strings
     *                pathnames relative to the testdata location. Directories will
     *                be copied recursively.
     * @throws IOException
     * @throws CoreException
     */
    public void install(final String[] strings) throws CoreException,
            IOException {
        IWorkspaceRunnable r = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
                try {
                    for (int i = 0; i < strings.length; i++) {
                        String string = strings[i];
                        install_impl(string);
                    }
                } catch (Throwable e) {
                    Debug.report(e);
                    throw new RuntimeException(e);
                }
            }
        };
        ResourcesPlugin.getWorkspace().run(r, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
    }

    /**
     * @param string
     * @throws CoreException
     */
    public void install(final String string) throws CoreException {
        IWorkspaceRunnable r = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
                try {
                    install_impl(string);
                } catch (Throwable e) {
                    Debug.report(e);
                    throw new RuntimeException(e);
                }
            }
        };
        ResourcesPlugin.getWorkspace().run(r, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
    }

    /**
     * @param string
     * @throws CoreException
     * @throws IOException
     */
    private void install_impl(String string) throws CoreException, IOException {
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
     *                pathnames relative to the testdata location. Directories will
     *                uninstalled recursively.
     * @throws CoreException
     */
    public void uninstall(final String[] strings) throws CoreException {
        IWorkspaceRunnable r = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
                try {

                    for (int i = 0; i < strings.length; i++) {
                        String string = strings[i];
                        uninstall_impl(string);
                    }
                } catch (Throwable e) {
                    Debug.report(e);
                    throw new RuntimeException(e);
                }

            }
        };
        ResourcesPlugin.getWorkspace().run(r, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
    }

    /**
     * @param string
     * @throws CoreException
     */
    public void uninstall(final String string) throws CoreException {
        IWorkspaceRunnable r = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {

                uninstall_impl(string);

            }
        };
        ResourcesPlugin.getWorkspace().run(r, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
    }

    /**
     * @param string
     * @throws CoreException
     */
    private void uninstall_impl(String string) throws CoreException {
        IResource r = getTestProject().findMember(string);
        uninstall_impl(r);
    }

    private void uninstall_impl(IResource r) throws CoreException {
        r.refreshLocal(IResource.DEPTH_INFINITE, null);
        //		if (r instanceof IContainer) {
        //			IContainer c = (IContainer) r;
        //			IResource[] resources = c
        //					.members(IContainer.INCLUDE_TEAM_PRIVATE_MEMBERS
        //							| IContainer.INCLUDE_PHANTOMS);
        //			for (int i = 0; i < resources.length; i++) {
        //				uninstall_impl(resources[i]);
        //			}
        //		}
        try {
            r.delete(true, null);
        } catch (Throwable e) {
            throw new RuntimeException("could not remove " + r.getFullPath(), e);
        }
        assertTrue(r.isSynchronized(IResource.DEPTH_INFINITE));
        assertFalse(r.exists());
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

    public void addNature(IProject project, String id) throws CoreException {
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

    public void removeNature(IProject project, String id) throws CoreException {
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
        ResourcesPlugin.getWorkspace().run(create, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
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
        ResourcesPlugin.getWorkspace().run(create, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
        return project;
    }

    private void __deleteFile(File file) {
        file = file.getAbsoluteFile();
        if (!file.exists())
            return;
        if (file.isDirectory()) {
            String[] files = file.list();
            //file.list() can return null
            if (files != null) {
                for (int i = 0; i < files.length; ++i) {
                    __deleteFile(new File(file, files[i]));
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

    private void deleteProject(String projectName) throws CoreException {
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
    private void deleteProjects(final String[] projectNames)
            throws CoreException {
        IWorkspaceRunnable r = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
                if (projectNames != null) {
                    for (int i = 0, max = projectNames.length; i < max; i++) {
                        if (projectNames[i] != null)
                            deleteProject(projectNames[i]);
                    }
                }
            }
        };
        ResourcesPlugin.getWorkspace().run(r, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
    }

    /**
     * Delete this resource.
     */
    private void deleteResource(IResource resource) throws CoreException {
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

    /*
     * adopted from org.cs3.jllmp.builders.FactBaseBuilder
     */
    private void collectAll(final IResource root,final Collection toProcess) throws CoreException {
       
        IProject project=root.getProject();
        final IJavaProject javaProject = (IJavaProject) project
                .getNature(JavaCore.NATURE_ID);

        root.accept(new IResourceVisitor() {

            public boolean visit(IResource resource) throws CoreException {

                Debug.debug("Visiting: " + resource);

                if (resource.getType() == IResource.ROOT)
                    return true;
                if (resource.getType() == IResource.PROJECT)
                    return resource.getProject().hasNature(JLMP.NATURE_ID);
                /*
                 * since we only enter Projects that have the JLMP Nature set,
                 * this should be safe...
                 */

                if (resource.getType() == IResource.FOLDER) {
                    return javaProject.isOnClasspath(resource);
                }
                if (resource.getType() == IResource.FILE) {
                    if (resource.getFileExtension() != null
                            && resource.getFileExtension().equals("java")) {
                        Debug.debug("Adding " + resource + " to toProcess");
                        toProcess.add(resource);
                    }
                }
                return false;
            }
        });
    }
    public ICompilationUnit[] getCompilationUnitsRecursive(String packageName)
            throws CoreException {
        List files = new Vector();
        IFolder folder = getTestProject().getFolder(packageName);
        folder.refreshLocal(IResource.DEPTH_INFINITE, null);
        collectAll(folder,files); 
        ICompilationUnit[] result = new ICompilationUnit[files.size()];
        for (int i=0;i<result.length;i++) {
            IFile file = (IFile) files.get(i);
            ICompilationUnit cu = (ICompilationUnit) JavaCore.create(file);
            result[i]=cu;
        }
        
        return result;
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

    public ICompilationUnit getCompilationUnitFor(IJavaElement element) {

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
        try{
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
        finally{
            icu.discardWorkingCopy();
        }
    }

    private void normalize(final ASTRewrite rewrite,
            VariableDeclarationStatement node) {
        List originalFragments = node.fragments();
        ASTNode parent = node.getParent();
        ChildListPropertyDescriptor statementsProperty=null;
        switch(parent.getNodeType()){
        	case ASTNode.BLOCK:
        	    statementsProperty = Block.STATEMENTS_PROPERTY;
        	break;
        	case ASTNode.SWITCH_STATEMENT:
        	    statementsProperty = SwitchStatement.STATEMENTS_PROPERTY;
        	break;
        	default:
        	    throw new IllegalArgumentException("could not handle parent node: "+parent.toString());
        }
        
        
        ASTNode last = node;
        for (int i = 1; i < originalFragments.size(); i++) {
            VariableDeclarationFragment origFrag = (VariableDeclarationFragment) originalFragments
                    .get(i);
            VariableDeclarationFragment movedFrag = (VariableDeclarationFragment) rewrite
                    .createMoveTarget(origFrag);
            rewrite.remove(origFrag, null);
            VariableDeclarationStatement newVarDecl = node.getAST()
                    .newVariableDeclarationStatement(movedFrag);
            newVarDecl.setType((Type) rewrite.createCopyTarget(node.getType()));
            newVarDecl.setModifiers(node.getModifiers());
            ListRewrite listRewrite = rewrite.getListRewrite(parent,
                    statementsProperty);
            listRewrite.insertAfter(newVarDecl, last, null);
            last = newVarDecl;
        }
    }

    private void normalize(final ASTRewrite rewrite, FieldDeclaration node) {
        List originalFragments = node.fragments();
        AbstractTypeDeclaration parent = (AbstractTypeDeclaration) node
                .getParent();
        ASTNode last = node;
        for (int i = 1; i < originalFragments.size(); i++) {
            VariableDeclarationFragment origFrag = (VariableDeclarationFragment) originalFragments
                    .get(i);
            VariableDeclarationFragment movedFrag = (VariableDeclarationFragment) rewrite
                    .createMoveTarget(origFrag);
            rewrite.remove(origFrag, null);
            FieldDeclaration newField = node.getAST().newFieldDeclaration(
                    movedFrag);
            newField.setType((Type) rewrite.createCopyTarget(node.getType()));
            newField.setModifiers(node.getModifiers());
            ListRewrite listRewrite = rewrite.getListRewrite(parent,
                    TypeDeclaration.BODY_DECLARATIONS_PROPERTY);
            listRewrite.insertAfter(newField, last, null);
            last = newField;
        }

    }

    private void normalize(ASTRewrite rewrite, ASTNode node) {
        switch (node.getNodeType()) {
        case ASTNode.EMPTY_STATEMENT:
            if (ASTNode.BLOCK == node.getParent().getNodeType()) {
                rewrite.remove(node, null);
            }
            break;
        case ASTNode.FIELD_DECLARATION:
            normalize(rewrite, (FieldDeclaration) node);
            break;
        case ASTNode.VARIABLE_DECLARATION_STATEMENT:
            normalize(rewrite, (VariableDeclarationStatement) node);
        }
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

        // creation of DOM/AST from a ICompilationUnit
        ASTParser parser = ASTParser.newParser(AST.JLS2);
        parser.setSource(cu);
        CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);

        List commentList = astRoot.getCommentList();
        /*
         * according to comments from usenet there is currently no way to modify
         * comments api-wise. So we have to be a little "creative" here.
         */
        Stack stack = new Stack();
        for (Iterator iter = commentList.iterator(); iter.hasNext();) {
            stack.push(iter.next());
        }

        IBuffer buffer = cu.getBuffer();
        while (!stack.isEmpty()) {
            Comment comment = (Comment) stack.pop();
            int start = comment.getStartPosition();
            int length = comment.getLength();
            buffer.replace(start, length, "");
        }
        //if we changed the cu, we have to reconcile...
        if (!commentList.isEmpty()) {
            astRoot = cu.reconcile(AST.JLS2, false, null, null);
        }

        //collect nodes that are known to cause trouble:
        final List toProcess = new Vector();
        astRoot.accept(new ASTVisitor() {

            public boolean visit(EmptyStatement node) {
                toProcess.add(node);
                return false;
            }

            public boolean visit(FieldDeclaration node) {
                toProcess.add(node);
                return false;
            }

            public boolean visit(VariableDeclarationStatement node) {
                toProcess.add(node);
                return false;
            }
        });

        //normalize collected nodes
        ASTRewrite rewrite = ASTRewrite.create(astRoot.getAST());
        for (Iterator iter = toProcess.iterator(); iter.hasNext();) {
            ASTNode node = (ASTNode) iter.next();
            normalize(rewrite, node);
        }

        // creation of a Document
        String source = buffer.getContents();
        Document document = new Document(source);

        //setup formatting options for the rewrite
        Map options = DefaultCodeFormatterConstants
                .getJavaConventionsSettings();

        options
                .put(DefaultCodeFormatterConstants.FORMATTER_LINE_SPLIT,
                        "99999");
        options
                .put(
                        DefaultCodeFormatterConstants.FORMATTER_NUMBER_OF_EMPTY_LINES_TO_PRESERVE,
                        "0");

        // computation of the text edits
        TextEdit edits = rewrite.rewriteAST(document, options);

        // computation of the new source code
        edits.apply(document);
        source = document.get();
        //the ASTRewrite tries to maintain as much formating as possible.
        //this is not what we want right now, so we have the source code
        // formater
        //run over the code once more.
        CodeFormatter formatter = ToolFactory.createCodeFormatter(options);
        edits = formatter.format(CodeFormatter.K_COMPILATION_UNIT, source, 0,
                source.length(), 0, System.getProperty("line.separator"));
        edits.apply(document);
        source = document.get();

        // final task: remove empty lines
        source = source.replaceAll("(?s)\\n\\s*\\n", "\n");

        // update of the compilation unit
        buffer.setContents(source);
        cu.commitWorkingCopy(false, null);

        //save and clean up.
        cu.save(null, true);
        cu.discardWorkingCopy();
        assertFalse(cu.isWorkingCopy());

    }

    /**
     * create a file with specified content within the runtime workspace.
     * 
     * @param path
     *                workspace relative path. Uses forward slashes ('/') as a file
     *                separator,
     * @param contents
     *                the contents of the file.
     * @return the created IFile
     * @throws CoreException
     */
    /**
     * @param filename
     * @param content
     * @return
     * @throws CoreException
     */
    public IResource createFile(final String filename, final String content)
            throws CoreException {
        IWorkspaceRunnable r = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
                try {
                    createFile_impl(filename, content);
                } catch (Throwable e) {
                    Debug.report(e);
                    throw new RuntimeException(e);
                }
            }
        };
        ResourcesPlugin.getWorkspace().run(r, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
        IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(
                new Path(filename));
        return file;
    }

    protected IFile createFile_impl(final String path, final String contents)
            throws CoreException {
        final Reader reader = new StringReader(contents);
        InputStream input = new InputStream() {
            public int read() throws IOException {
                return reader.read();
            }
        };
        IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(
                new Path(path));
        if (!file.isSynchronized(IResource.DEPTH_INFINITE)) {
            file.refreshLocal(IResource.DEPTH_INFINITE, null);
        }
        try {

            if (file.exists()) {
                assertTrue(file.isSynchronized(IResource.DEPTH_INFINITE));
                assertFalse(file.isReadOnly());
                assertTrue(file.isLocal(IResource.DEPTH_INFINITE));
                assertFalse(file.isPhantom());
                assertTrue(file.isAccessible());
                file.delete(true, false, null);
            }

        } catch (Throwable dummeNuttenScheisse) {
            Debug.debug("bla bla:" + file.getFullPath());
            throw new RuntimeException(dummeNuttenScheisse);
        }
        if (!file.isSynchronized(IResource.DEPTH_INFINITE)) {
            file.refreshLocal(IResource.DEPTH_INFINITE, null);
        }
        assertTrue(file.isSynchronized(IResource.DEPTH_INFINITE));
        assertFalse(file.exists());
        file.create(input, true, null);
        assertTrue(file.isAccessible());
        file.getParent().refreshLocal(IResource.DEPTH_INFINITE, null);
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

    protected void build(final String builder, final Map args)
            throws CoreException {
        final _ProgressMonitor m = new _ProgressMonitor();
        IProject project = getTestProject();
        project.build(IncrementalProjectBuilder.FULL_BUILD, builder, args, m);
        project.refreshLocal(IResource.DEPTH_INFINITE, null);
        m.waitTillDone();
    }

    protected void build(int kind) throws CoreException {
        final IProject project = getTestProject();
        _ProgressMonitor m = new _ProgressMonitor();
        project.build(kind, m);
        m.waitTillDone();
        project.refreshLocal(IResource.DEPTH_INFINITE, null);
    }

    protected void build() throws CoreException {
        final IProject project = getTestProject();
        _ProgressMonitor m = new _ProgressMonitor();
        project.build(IncrementalProjectBuilder.FULL_BUILD, m);
        m.waitTillDone();
        project.refreshLocal(IResource.DEPTH_INFINITE, null);
    }

    private void generateSource_impl() throws CoreException, IOException {
        String query = "toplevelT(ID,_,FILENAME,_),gen_tree(ID,CONTENT)";
        PrologSession session = getTestJLMPProject().getPrologInterface()
                .getSession();
        List results = null;
        try {
            results = session.queryAll(query);

            for (Iterator iter = results.iterator(); iter.hasNext();) {
                Map result = (Map) iter.next();
                String filename = result.get("FILENAME").toString();
                String content = result.get("CONTENT").toString();
                //clean the facts right away. another testcase might be running
                //concurrently and this is the easiest way to keep things from
                // interfering
                session.queryOnce("remove_contained_global_ids('" + filename
                        + "')");
                session.queryOnce("delete_toplevel('" + filename + "')");
                IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(
                        new Path(filename));
                file.create(new ByteArrayInputStream(content.getBytes()), true,
                        null);
                assertTrue(file.isSynchronized(IResource.DEPTH_INFINITE));
                assertTrue(file.exists());

                //just a test ...
                file.delete(true, false, null);
                assertTrue(file.isSynchronized(IResource.DEPTH_INFINITE));
                assertFalse(file.exists());
                file.create(new ByteArrayInputStream(content.getBytes()), true,
                        null);
                assertTrue(file.isSynchronized(IResource.DEPTH_INFINITE));
                assertTrue(file.exists());

            }
        } catch (PrologException e) {
            throw new RuntimeException(e);
        } finally {
            session.dispose();
        }
    }

    protected void generateSource() throws CoreException, IOException {
        IWorkspaceRunnable r = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
                try {
                    generateSource_impl();

                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        };
        ResourcesPlugin.getWorkspace().run(r, getTestProject(),
                IWorkspace.AVOID_UPDATE, null);
    }

    private void _normalize(final ASTRewrite rewrite,
            VariableDeclarationStatement node) {
        List newFragments = new Vector();

        List oldFragments = node.fragments();
        Block parent = (Block) node.getParent();
        if (oldFragments.size() > 1) {
            for (Iterator iter = oldFragments.iterator(); iter.hasNext();) {
                VariableDeclarationFragment oldFragment = (VariableDeclarationFragment) iter
                        .next();
                rewrite.remove(oldFragment, null);
                VariableDeclarationFragment newFragment = (VariableDeclarationFragment) rewrite
                        .createMoveTarget(oldFragment);
                newFragments.add(newFragment);
            }
            ASTNode last = node;
            for (Iterator iter = newFragments.iterator(); iter.hasNext();) {
                VariableDeclarationFragment newFragment = (VariableDeclarationFragment) iter
                        .next();
                AST ast = node.getAST();
                VariableDeclarationStatement newVarDecl = (VariableDeclarationStatement) rewrite
                        .createCopyTarget(node);
                ListRewrite listRewrite = rewrite.getListRewrite(newVarDecl,
                        VariableDeclarationStatement.FRAGMENTS_PROPERTY);
                listRewrite.insertFirst(newFragment, null);
                listRewrite = rewrite.getListRewrite(parent,
                        Block.STATEMENTS_PROPERTY);
                listRewrite.insertAfter(newVarDecl, last, null);
                last = newVarDecl;
            }
            rewrite.remove(node, null);
        }
    }
}

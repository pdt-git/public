package org.cs3.pl.astvisitor;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IProblemRequestor;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.compiler.IProblem;
import org.eclipse.jdt.core.dom.CompilationUnit;
/**
 * Generates CompilationUnit and ICompilationUnit objects,
 * and if init(IProject) has been called, can also generate 
 * Iterators over the current Project. These Methods should be
 * considered discouraged.
 * 
 * @author degenerl
 * @inheritDoc
 */
public class CompilationUnitProvider
		implements
			ICompilationUnitProvider,
			IResourceVisitor {
	private Vector sourceFiles = new Vector();
	private IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
	private IProgressMonitor internalMonitor = new NullProgressMonitor();
	private IProgressMonitor monitor = null;
	private boolean initialized = false;
	private int hash;
	
	/**
	 * constructs a new CompilationUnitProvider.
	 *
	 */
	public CompilationUnitProvider(){
		
	}

	/**
	 * constructs a new CompilationUnitProvider, causing it
	 * to search for all Java Source files in the passed Project
	 * 
	 * @deprecated obsolete
	 * @param project The project to search for source files
	 */
	
	public CompilationUnitProvider(IProject project) {
		sourceFiles.clear();
		try {
			project.accept(this);
			this.initialized = true;
		} catch (CoreException e) {
			Debug.report(e);
		}
		this.hash = sourceFiles.hashCode();
	}

	public List getSourceFiles() {
		if (!initialized) {
			throw new IllegalStateException(
					"The CUProvider has not been initialized!!");
		}
		return sourceFiles;
	}
	private class _ICUIterator implements Iterator {
		private Iterator fileIterator;
		private int hash;
		public _ICUIterator(Iterator fileIterator, int hash) {
			this.fileIterator = fileIterator;
			this.hash = hash;
		}
		public void remove() {
			throw new UnsupportedOperationException(
					"WHAT exactly are you trying to remove, dude??");
		}
		public boolean hasNext() {
			if (this.hash != CompilationUnitProvider.this.hash) {
				throw new IllegalStateException(
						"iterator has been invalidated!");
			}
			return fileIterator.hasNext();
		}
		public Object next() {
			if (this.hash != CompilationUnitProvider.this.hash) {
				throw new IllegalStateException(
						"iterator has been invalidated!");
			}
			return createICompilationUnit((IFile) fileIterator.next());
		}
	}
	
	
	private class _CUIterator implements Iterator {
		private Iterator fileIterator;
		private int hash;
		public _CUIterator(Iterator fileIterator, int hash) {
			this.fileIterator = fileIterator;
			this.hash = hash;
		}
		public void remove() {
			throw new UnsupportedOperationException(
					"WHAT exactly are you trying to remove, dude??");
		}
		public boolean hasNext() {
			if (this.hash != CompilationUnitProvider.this.hash) {
				throw new IllegalStateException(
						"iterator has been invalidated!");
			}
			return fileIterator.hasNext();
		}
		public Object next() {
			if (this.hash != CompilationUnitProvider.this.hash) {
				throw new IllegalStateException(
						"iterator has been invalidated!");
			}
			return createCompilationUnit((IFile) fileIterator.next());
		}
	}
	
	public Iterator getCUIterator() {
		if (!initialized) {
			throw new IllegalStateException(
					"The CUProvider has not been initialized!!");
		}
		return new _CUIterator(sourceFiles.iterator(), hash);
	}
	// end of implementation of Interface ICompilationUnitProvider
	//////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////
	// implementation of Interface IResourceVisitor
	public boolean visit(IResource resource) throws CoreException {
		boolean descent = false;
		switch (resource.getType()) {
			case IResource.FILE :
				IFile file = (IFile) resource;
				if (!file.isAccessible())
					throw new IllegalStateException(
							"the file is not accessible:" + file);
				if (file.getFileExtension() == null) {
					descent = false;
					break;
				}
				if (file.getFileExtension().equals("java")
						|| file.getFileExtension().equals(".jav")) {
					if (monitor != null) {
						monitor.subTask(file.getFullPath().toString());
					}
					sourceFiles.add(file);
				}
				break;
			case IResource.FOLDER :
				descent = true;
				break;
			case IResource.ROOT :
				descent = true;
				break;
			case IResource.PROJECT :
				descent = true;
				break;
			default :
				throw new IllegalStateException("The Resource (" + resource
						+ ") is of an unexpected type:" + resource.getType());
		}
		return descent;
	}
	// end of implementation of Interface IResourceVisitor
	//////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////
	// only local helper methods below :-)
	///////////////////////////
	protected CompilationUnit createCompilationUnit(IFile file) {
		IProject project = file.getProject();
		IPath path = file.getFullPath();
		IPath srcPath = null;
		IPath binPath = null;
		if (path.segment(1).equalsIgnoreCase("src")) {
			srcPath = new Path(path.segment(0) + "/src");
			binPath = new Path(path.segment(0) + "/bin");
		} else {
			srcPath = new Path(path.segment(0));
			binPath = srcPath;
		}
		String[] natureIds = null;
		IProjectDescription description = null;
		try {
			description = project.getDescription();
			natureIds = project.getDescription().getNatureIds();
		} catch (CoreException e2) {
			Debug.report(e2);
		}
		boolean hasJavaNature = false;
		for (int i = 0; i < natureIds.length; i++) {
			if (natureIds[i].equals(JavaCore.NATURE_ID)) {
				hasJavaNature = true;
				break;
			}
		}
		IJavaProject javaProject = JavaCore.create(project);
		if (!hasJavaNature) {
			Debug.info("adding Java nature to project: "
					+ JavaCore.NATURE_ID);
			String[] newNatureIds = new String[natureIds.length + 1];
			System.arraycopy(natureIds, 0, newNatureIds, 0, natureIds.length);
			newNatureIds[natureIds.length] = JavaCore.NATURE_ID;
			try {
				description.setNatureIds(newNatureIds);
				project.setDescription(description, IResource.FORCE,
						internalMonitor);
				//project.build(IncrementalProjectBuilder.FULL_BUILD,monitor);
				root.refreshLocal(IResource.DEPTH_INFINITE, internalMonitor);
			} catch (CoreException e3) {
				// TODO Auto-generated catch block
				Debug.report(e3);
			}
			IPath magicSrcPath = srcPath.makeAbsolute();
			IPath magicBinPath = binPath.makeAbsolute();
			////System.out.println(magicSrcPath);
			IClasspathEntry[] newClasspath = new IClasspathEntry[2];
			try {
				newClasspath[0] = JavaCore.newSourceEntry(magicSrcPath);
				newClasspath[1] = JavaCore.newContainerEntry(new Path(
						"org.eclipse.jdt.launching.JRE_CONTAINER"));
				//newClasspath[0] = JavaCore.newSourceEntry(magicBinPath);
			} catch (Exception ex) {
				////System.out.println("Nämlich:");
				Debug.report(ex);
			}
			try {
				javaProject.setRawClasspath(newClasspath, internalMonitor);
				javaProject.setOutputLocation(magicBinPath, internalMonitor);
			} catch (JavaModelException e4) {
				// TODO Auto-generated catch block
				Debug.report(e4);
			}
		}
		//		try {
		//			System.out.println(javaProject.getRawClasspath());
		//		} catch (JavaModelException e1) {
		//			// TODO Auto-generated catch block
		//			Debug.report(e1);
		//		}
		ICompilationUnit icu = (ICompilationUnit) JavaCore.create(file);
		try {
			icu.becomeWorkingCopy(new IProblemRequestor() {
				public void acceptProblem(IProblem arg0) {
					//ld says: shut up and dance.
					if (arg0.isWarning())
						return;//;-)
					String message = arg0.getMessage();
					String file = new String(arg0.getOriginatingFileName());
					int line = arg0.getSourceLineNumber();
					Debug.error("error: in file " + file + " at line "
							+ line + ":");
					Debug.error(message);
				}
				public void beginReporting() {

				}
				public void endReporting() {
				}
				public boolean isActive() {
					return true;
				}
			}, null);
		} catch (JavaModelException e) {
			Debug.report(e);
		}
		CompilationUnit root = PDTPlugin.getDefault().parseICompilationUnit(icu);
		/*
		 * try { ////System.out.println("icu: " + icu + " root: " + root + "
		 * file: " //+ icu.getCorrespondingResource()); } catch
		 * (JavaModelException e3) { // TODO Auto-generated catch block
		 * Debug.report(e3);
		 */
		return root;
	}
	public ICompilationUnit createICompilationUnit(IFile file) {
		IProject project = file.getProject();
		IPath path = file.getFullPath();
		IPath srcPath = null;
		IPath binPath = null;
		if (path.segment(1).equalsIgnoreCase("src")) {
			srcPath = new Path(path.segment(0) + "/src");
			binPath = new Path(path.segment(0) + "/bin");
		} else {
			srcPath = new Path(path.segment(0));
			binPath = srcPath;
		}
		String[] natureIds = null;
		IProjectDescription description = null;
		try {
			description = project.getDescription();
			natureIds = project.getDescription().getNatureIds();
		} catch (CoreException e2) {
			Debug.report(e2);
		}
		boolean hasJavaNature = false;
		for (int i = 0; i < natureIds.length; i++) {
			if (natureIds[i].equals(JavaCore.NATURE_ID)) {
				hasJavaNature = true;
				break;
			}
		}
		IJavaProject javaProject = JavaCore.create(project);
		if (!hasJavaNature) {
			Debug.info("adding Java nature to project: "
					+ JavaCore.NATURE_ID);
			String[] newNatureIds = new String[natureIds.length + 1];
			System.arraycopy(natureIds, 0, newNatureIds, 0, natureIds.length);
			newNatureIds[natureIds.length] = JavaCore.NATURE_ID;
			try {
				description.setNatureIds(newNatureIds);
				project.setDescription(description, IResource.FORCE,
						internalMonitor);
				//project.build(IncrementalProjectBuilder.FULL_BUILD,monitor);
				root.refreshLocal(IResource.DEPTH_INFINITE, internalMonitor);
			} catch (CoreException e3) {
				Debug.report(e3);
			}
			IPath magicSrcPath = srcPath.makeAbsolute();
			IPath magicBinPath = binPath.makeAbsolute();
			////System.out.println(magicSrcPath);
			IClasspathEntry[] newClasspath = new IClasspathEntry[2];
			try {
				newClasspath[0] = JavaCore.newSourceEntry(magicSrcPath);
				newClasspath[1] = JavaCore.newContainerEntry(new Path(
						"org.eclipse.jdt.launching.JRE_CONTAINER"));
				//newClasspath[0] = JavaCore.newSourceEntry(magicBinPath);
			} catch (Exception ex) {
				Debug.report(ex);
			}
			try {
				javaProject.setRawClasspath(newClasspath, internalMonitor);
				javaProject.setOutputLocation(magicBinPath, internalMonitor);
			} catch (JavaModelException e4) {
				Debug.report(e4);
			}
		}
		ICompilationUnit icu = (ICompilationUnit) JavaCore.create(file);
		return icu;
	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.astvisitor.ICompilationUnitProvider#getICUIterator()
	 */
	public Iterator getICUIterator() {
		if (!initialized) {
			throw new IllegalStateException(
					"The CUProvider has not been initialized!!");
		}
		return new _ICUIterator(sourceFiles.iterator(), hash);

	}
}

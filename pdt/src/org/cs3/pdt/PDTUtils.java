package org.cs3.pdt;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;

public final class PDTUtils {

	
		
	public static IPath normalize(IPath path) {
		IPath testLocation = null;
		try {

			testLocation = new Path(path.toFile().getCanonicalFile().toString());
		} catch (IOException e1) {
			Debug.report(e1);
			throw new RuntimeException(e1);
		}
		return testLocation;
	}

	/**
	 * adapted from
	 * org.eclipse.core.internal.localstore.FileSystemResourceManager. This is
	 * the "corrected" version: it does normalize the locations before comparing
	 * them. propably hurts performance, but i cant help it. --lu
	 */
	public static List allPathsForLocation(IPath l) {
		IPath location = normalize(l);
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		final ArrayList results = new ArrayList();
		for (int i = 0; i < projects.length; i++) {
			IProject project = projects[i];
			// check the project location

			IPath testLocation = normalize(project.getLocation());
			IPath suffix;
			if (testLocation != null && testLocation.isPrefixOf(location)) {
				suffix = location.removeFirstSegments(testLocation
						.segmentCount());
				results.add(project.getFullPath().append(suffix));
			}
			if (!project.isAccessible())
				continue;
			IResource[] children = null;
			try {
				children = project.members();
			} catch (CoreException e) {
				// ignore projects that cannot be accessed
			}
			if (children == null)
				continue;
			for (int j = 0; j < children.length; j++) {
				IResource child = children[j];
				if (child.isLinked()) {
					testLocation = normalize(child.getLocation());
					if (testLocation != null
							&& testLocation.isPrefixOf(location)) {
						// add the full workspace path of the corresponding
						// child of the linked resource
						suffix = location.removeFirstSegments(testLocation
								.segmentCount());
						results.add(child.getFullPath().append(suffix));
					}
				}
			}
		}
		return results;
	}

	 /**
	 * @param file
	 * @return
	 * @throws IOException
	 */
	public static IFile[] findFilesForLocation(String path) {
		IPath fpath = new Path(path);
		return findFilesForLocation(fpath);
	}

	public static IFile[] findFilesForLocation(IPath location) {
		IFile file = null;
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();

		List list = PDTUtils.allPathsForLocation(location);
		ArrayList result = new ArrayList(list.size());
		for (Iterator it = list.iterator(); it.hasNext();) {
			IPath p = (IPath) it.next();
			IResource r = root.findMember(p);
			if (r != null && r.getType() == IResource.FILE) {
				result.add(r);
			}
		}
		IFile[] files = (IFile[]) result.toArray(new IFile[result.size()]);
		return files;

	}

	/**
	 * @param file
	 * @return
	 * @throws IOException
	 */
	public static IFile findFileForLocation(String path) throws IOException {
	    
	    return findFileForLocation(new Path(path));
	}

	public static IFile findFileForLocation(Path path) {
		IFile file = null;
	    IFile[] files = findFilesForLocation(path);
	    if (files == null || files.length == 0) {
	        throw new IllegalArgumentException("Not in Workspace: " + path);            
	    }
	    if (files.length > 1) {
	        Debug.warning("Mapping into workspace is ambiguous:" + path);
	        Debug.warning("i will use the first match found: " + files[0]);
	    }
	    file = files[0];
	    if (!file.isAccessible()) {
	        throw new RuntimeException("The specified file \"" + file
	                + "\" is not accessible.");
	    }
	    return file;		
	}

	public static PrologInterface getActiveConsolePif(){
		return PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole().getPrologInterface();
	}
	
	
	public static void showSourceLocation(final SourceLocation loc) {
		if(Display.getCurrent()!=UIUtils.getDisplay()){
			UIUtils.getDisplay().asyncExec(new Runnable() {			
				public void run() {
					showSourceLocation(loc);
				}			
			});
			return;
		}
		
	    IFile file=null;   
		IPath fpath= new Path(loc.file);
		IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		
		//see if it is a workspace path:
		file = wsRoot.getFile(fpath );
		
		boolean showLine;
		if(!loc.isWorkspacePath) {
			try {
				file = findFileForLocation(loc.file);
			} catch (IOException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}
	    
	    IEditorPart part;
		
	    try {
	        IWorkbenchPage page=UIUtils.getActivePage();
			part = IDE.openEditor(page, file);
	    } catch (PartInitException e) {
	        Debug.report(e);
	        return;
	    }
	    if (part instanceof PLEditor) {
	        PLEditor editor = (PLEditor) part;
	      
			if(loc.isRowBased)
				editor.gotoLine(loc.line);
			else
				editor.gotoOffset(loc.offset);
	    }
		
	}

	
}
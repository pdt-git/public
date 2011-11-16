/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 * 
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 * 
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.core;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.eclipse.core.filebuffers.FileBuffers;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.filebuffers.ITextFileBufferManager;
import org.eclipse.core.filebuffers.LocationKind;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
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
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.AbstractTextEditor;

public final class PDTCoreUtils {

	public static void addPDTNature(IProject project) throws CoreException {

		IProjectDescription ipd = project.getDescription();
		String[] oldNIDs = ipd.getNatureIds();
		String[] newNIDs = new String[oldNIDs.length + 1];
		newNIDs[0] = PDTCore.NATURE_ID;
		System.arraycopy(oldNIDs, 0, newNIDs, 1, oldNIDs.length);
		ipd.setNatureIds(newNIDs);
		if (!project.isSynchronized(IResource.DEPTH_ONE)) {
			project.refreshLocal(IResource.DEPTH_ONE, null);
		}
		project.setDescription(ipd, null);

	}

	public static void removePDTNature(IProject project) throws CoreException {
		if (project.hasNature(PDTCore.NATURE_ID)) {
			IProjectDescription ipd = project.getDescription();
			String[] oldNIDs = ipd.getNatureIds();
			String[] newNIDs;
			newNIDs = new String[oldNIDs.length - 1];
			int j = 0;
			for (int i = 0; i < newNIDs.length; i++) {
				if (oldNIDs[j].equals(PDTCore.NATURE_ID)) {
					j++;
				}
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

	public static int convertLogicalToPhysicalOffset(String data, int offset) {
		String value = PDTCorePlugin.getDefault().getPreferenceValue(
				PDTCore.PREF_CONVERT_CHARACTER_OFFSETS, "true");
		if ("true".equalsIgnoreCase(value))
			return Util.logicalToPhysicalOffset(data, offset);
		return offset;
	}

	public static int convertPhysicalToLogicalOffset(String data, int offset) {
		String value = PDTCorePlugin.getDefault().getPreferenceValue(
				PDTCore.PREF_CONVERT_CHARACTER_OFFSETS, "true");
		if ("true".equalsIgnoreCase(value))
			return Util.physicalToLogicalOffset(data, offset);
		return offset;
	}

	public static int convertLogicalToPhysicalOffset(IDocument doc, int offset) {
		return convertLogicalToPhysicalOffset(doc.get(), offset);
	}

	public static int convertPhysicalToLogicalOffset(IDocument doc, int offset) {
		return convertPhysicalToLogicalOffset(doc.get(), offset);
	}

	public static IDocument getDocument(IFile file) throws CoreException{
		IPath path = file.getFullPath();
		return getDocument(path,LocationKind.IFILE);
	}

	public static IDocument getDocument(File file) throws CoreException{
		IPath path = new Path(file.getAbsolutePath());
		return getDocument(path,LocationKind.NORMALIZE);
	}

	public static IDocument getDocument(IPath location, LocationKind kind) throws CoreException{
		ITextFileBufferManager manager= FileBuffers.getTextFileBufferManager();
		try {
			manager.connect(location, kind,null);
			ITextFileBuffer buffer= manager.getTextFileBuffer(location,kind);
			// note: could be null
			return buffer.getDocument();
		}
		finally {
			manager.disconnect(location, kind,null);
		}
	}




	public static IPrologProject getPrologProject(IResource file) throws CoreException {
		if (file == null)
			return null;
		IProject project = file.getProject();

		if ((project != null) &&project.isOpen()&& project.hasNature(PDTCore.NATURE_ID))
			return (IPrologProject) project.getNature(PDTCore.NATURE_ID);
		else
			return null;
	}

	/**
	 * adapted from
	 * org.eclipse.core.internal.localstore.FileSystemResourceManager. This is
	 * the "corrected" version: it does normalize the locations before comparing
	 * them. propably hurts performance, but i cant help it. --lu
	 */
	public static List<IPath> allPathsForLocation(IPath l) {
		IPath location = normalize(l);
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		final ArrayList<IPath> results = new ArrayList<IPath>();
		for (IProject project2 : projects) {
			IProject project = project2;
			// check the project location

			IPath testLocation = normalize(project.getLocation());
			IPath suffix;
			if ((testLocation != null) && testLocation.isPrefixOf(location)) {
				suffix = location.removeFirstSegments(testLocation
						.segmentCount());
				results.add(project.getFullPath().append(suffix));
			}
			if (!project.isAccessible()) {
				continue;
			}
			IResource[] children = null;
			try {
				children = project.members();
			} catch (CoreException e) {
				// ignore projects that cannot be accessed
			}
			if (children == null) {
				continue;
			}
			for (IResource child : children) {
				if (child.isLinked()) {
					testLocation = normalize(child.getLocation());
					if ((testLocation != null)
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

	public static IFile[] findFilesForLocation(String path) {
		IPath fpath = new Path(path);
		return findFilesForLocation(fpath);
	}

	public static IFile[] findFilesForLocation(IPath location) {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();

		List<IPath> list = PDTCoreUtils.allPathsForLocation(location);
		ArrayList<IResource> result = new ArrayList<IResource>(list.size());
		for (IPath p : list) {
			IResource r = root.findMember(p);
			if ((r != null) && (r.getType() == IResource.FILE)) {
				result.add(r);
			}
		}
		IFile[] files = result.toArray(new IFile[result.size()]);
		return files;

	}


	public static IFile findFileForLocation(String path) throws IOException {
		return findFileForLocation(new Path(path));
	}

	public static IFile findFileForLocation(Path path) {
		IFile file = null;
		IFile[] files = findFilesForLocation(path);
		if ((files == null) || (files.length == 0))
			throw new IllegalArgumentException("Not in Workspace: " + path);
		if (files.length > 1) {
			Debug.warning("Mapping into workspace is ambiguous:" + path);
			Debug.warning("i will use the first match found: " + files[0]);
		}
		file = files[0];
		if (!file.isAccessible())
			throw new RuntimeException("The specified file \"" + file
					+ "\" is not accessible.");
		return file;
	}

	/**
	 * Returns a file even if it is not part of the current Workspace.
	 * 
	 * @param fileName
	 * @return
	 * @throws IOException
	 */
	public static IFile getFileForLocationIndependentOfWorkspace(String fileName)
			throws IOException {
		IFile file = null;
		String path = Util.unquoteAtom(fileName);
		try{
			file = findFileForLocation(path);
		}catch(IllegalArgumentException iae){
		}
		if((file==null)|| !file.isAccessible()){
			Path location = new Path(path);
			file = new ExternalFile(location);
		}
		return file;
	}

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

	public static IFile[] sortFileSet(IFile[] sortedFiles) {
		Arrays.sort(sortedFiles,
				new Comparator<IFile>() {
			@Override
			public int compare(IFile first, IFile second) {
				return first.getFullPath().toPortableString().compareTo(second.getFullPath().toPortableString());
			}
		}
				);
		return sortedFiles;
	}

	/**
	 * Select a text region in file filename which starts at offset start with
	 * length length.
	 * 
	 * @param start
	 * @param length
	 * @param filename
	 * @throws PartInitException
	 * @throws FileNotFoundException
	 */
	static public void selectInEditor(int start, int length, String filename)
			throws PartInitException, FileNotFoundException {

		java.io.File f = new java.io.File(filename);


		Path path;
		try {
			path = new Path(f.getCanonicalPath());
			IFile file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(path);
			if (file == null){
				IFileStore fileStore = EFS.getLocalFileSystem().getStore(path);
				if (!fileStore.fetchInfo().isDirectory() && fileStore.fetchInfo().exists()) {
					try {
						IWorkbenchPage page = UIUtils.getActivePage();
				        IEditorPart part = IDE.openEditorOnFileStore(page, fileStore);
						if (part instanceof AbstractTextEditor){
					        IDocument document = ((AbstractTextEditor)part).getDocumentProvider().getDocument(part.getEditorInput());
							int end = convertLogicalToPhysicalOffset(
									document, start+length);
							start = convertLogicalToPhysicalOffset(
									document, start);
							length = end - start;
							ISelection selection = new TextSelection(document,start,length);
							part.getEditorSite().getSelectionProvider().setSelection(selection);
						}
					} catch (PartInitException e) {
						Debug.report(e);
					}
					return;
				}

//				throw new FileNotFoundException("could not find the file: '"
//						+ filename + "' in the workspace.");
			} else {

				UIUtils.openInEditor(file, false);
				IDocument document = ((AbstractTextEditor) UIUtils.getActiveEditor())
						.getDocumentProvider().getDocument(
								UIUtils.getActiveEditor().getEditorInput());

				int end = convertLogicalToPhysicalOffset(
						document, start+length);
				start = convertLogicalToPhysicalOffset(
						document, start);
				length = end - start;

				ISelection selection = new TextSelection(document, start, length);
				UIUtils.getActiveEditor().getEditorSite().getSelectionProvider()
				.setSelection(selection);

			}
		} catch (IOException e) {
			Debug.report(e);
		}


		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				UIUtils.getActiveEditor().setFocus();
			}
		});

	}
}

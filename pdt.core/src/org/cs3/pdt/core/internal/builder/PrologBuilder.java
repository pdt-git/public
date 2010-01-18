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

/*
 */
package org.cs3.pdt.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;

public class PrologBuilder extends IncrementalProjectBuilder {


	public PrologBuilder() {
		super();
	}

	private void contentsChanged(IFile file, AsyncPrologSession as)
			throws CoreException, PrologInterfaceException {
		Debug.debug("builder processing file " + file);
		File ioFile = file.getLocation().toFile();
		String plFileName = Util.prologFileName(ioFile);
		file.getCharset();
		as.queryOnce(file, "pdt_file_contents_changed('" + plFileName + "')");
	}

	private void existenceChanged(IFile file, AsyncPrologSession as)
			throws CoreException, PrologInterfaceException {
		Debug.debug("builder processing file " + file);
		File ioFile = file.getLocation().toFile();
		String plFileName = Util.prologFileName(ioFile);
		file.getCharset();
		as.queryOnce(file, "pdt_file_existence_changed('" + plFileName + "')");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
	 *      java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	protected IProject[] build(int kind, Map args,
			final IProgressMonitor monitor) throws CoreException {

		try {

			Debug.debug("PrologBuilder.build(...) was triggered");
			String taskname = "updating prolog metadata";

			/*
			 * from the pdt's pov there are two ways a file may change its
			 * existence may be affected (add/remove) its contents may change
			 */
			Set<IFile> existenceList = new HashSet<IFile>();
			Set<IFile> contentList = new HashSet<IFile>();

			switch (kind) {
			case IncrementalProjectBuilder.AUTO_BUILD:
			case IncrementalProjectBuilder.INCREMENTAL_BUILD:

				collect(getDelta(getProject()), contentList, existenceList);
				break;
			case IncrementalProjectBuilder.FULL_BUILD:
				getProject().deleteMarkers(IMarker.PROBLEM, true,
						IResource.DEPTH_INFINITE);
				collect(getProject(), existenceList);
				break;
			case IncrementalProjectBuilder.CLEAN_BUILD:
				getProject().deleteMarkers(IMarker.PROBLEM, true,
						IResource.DEPTH_INFINITE);
				collect(getProject(), existenceList);
				break;
			default:
				Debug.error("Wasn das f�r ein Buil kind jetzt?");
				return null;
			}

			IPrologProject plProject = (IPrologProject) getProject().getNature(
					PDTCore.NATURE_ID);

			PrologInterface pif = ((PrologInterface) plProject
					.getMetadataPrologInterface());
			setupPif(pif);
			final AsyncPrologSession as = pif.getAsyncSession(PrologInterface.NONE);

			monitor.beginTask(taskname, contentList.size()
					+ existenceList.size());
			as.addBatchListener(new DefaultAsyncPrologSessionListener() {
				public void goalSucceeded(AsyncPrologSessionEvent e) {
					if (e.ticket instanceof IFile) {
						monitor.worked(1);
					}
					if (monitor.isCanceled()) {
						new Thread() {
							public void run() {
								as.dispose();
								Debug
										.debug("prolog builder aborted (theoretically)");
							}
						}.start();

					}
				}

				public void batchError(AsyncPrologSessionEvent e) {
					// XXX: not sure if this is the right way to do it...
					monitor.setCanceled(true);

				}
			});
			try {

				build(contentList,existenceList, as, monitor);

				if (!monitor.isCanceled()) {
					as.join();
				}
			} finally {
				if (as != null) {
					as.dispose();

				}
			}
			if (monitor.isCanceled()) {
				return null;
			}

			Debug.debug("PrologBuilder.build(...) is done.");
			monitor.done();
			plProject.updateMarkers();

			return null;
		} catch (OperationCanceledException e) {
			throw e;

		} catch (PrologInterfaceException e) {
			IStatus status = UIUtils
					.createErrorStatus(PDTCorePlugin.getDefault()
							.getErrorMessageProvider(), e, PDTCore.ERR_PIF);
			throw new CoreException(status);
		} catch (Throwable t) {
			IStatus status = UIUtils.createErrorStatus(PDTCorePlugin
					.getDefault().getErrorMessageProvider(), t,
					PDTCore.ERR_UNKNOWN);
			throw new CoreException(status);
		}
	}

	private void setupPif(PrologInterface pif) throws CoreException {
		PrologSession s = null;
		try {
			s = pif.getSession(PrologInterface.NONE);

			PrologLibraryManager mgr = PrologRuntimePlugin.getDefault()
					.getLibraryManager();
			PLUtil.configureFileSearchPath(mgr, s,
					new String[] { PrologRuntime.LIB_PDT });

			s.queryOnce("use_module(library('facade/pdt_workspace'))");
		} catch (PrologInterfaceException e) {
			IStatus status = UIUtils
					.createErrorStatus(PDTCorePlugin.getDefault()
							.getErrorMessageProvider(), e, PDTCore.ERR_PIF);
			throw new CoreException(status);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

	private void build(Set<IFile> contentsSet, Set<IFile> existenceSet,
			AsyncPrologSession as, IProgressMonitor monitor)
			throws CoreException, IOException, PrologInterfaceException {

		for (Iterator<IFile> it = existenceSet.iterator(); !monitor.isCanceled()
				&& it.hasNext();) {
			IFile file =it.next();
			existenceChanged(file, as);
		}
		for (Iterator<IFile> it = contentsSet.iterator(); !monitor.isCanceled()
				&& it.hasNext();) {
			IFile file =it.next();
			contentsChanged(file, as);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IncrementalProjectBuilder#clean(org.eclipse.core.runtime.IProgressMonitor)
	 */
	protected void clean(IProgressMonitor monitor) throws CoreException {
		Set<IFile> forgetList = new HashSet<IFile>();
		collect(getProject(), forgetList);
		getProject().deleteMarkers(IMarker.PROBLEM, true,
				IResource.DEPTH_INFINITE);
		IPrologProject plProject = (IPrologProject) getProject().getNature(
				PDTCore.NATURE_ID);
		AsyncPrologSession as = null;
		try {
			as = ((PrologInterface) plProject.getMetadataPrologInterface())
					.getAsyncSession(PrologInterface.NONE);

			forget(forgetList, as, monitor);
		} catch (PrologInterfaceException e) {
			IStatus status = UIUtils
					.createErrorStatus(PDTCorePlugin.getDefault()
							.getErrorMessageProvider(), e, PDTCore.ERR_PIF);
			throw new CoreException(status);
		} finally {
			if (as != null) {
				as.dispose();
			}
		}
	}

	/**
	 * @param project
	 * @param buildList
	 * @return
	 * @throws CoreException
	 */
	private void collect(IProject project, final Set<IFile> buildList)
			throws CoreException {
		final IPrologProject plProject = (IPrologProject) project
				.getNature(PDTCore.NATURE_ID);
		Set<IContainer> roots = plProject.getExistingSourcePathEntries();
		for (Iterator<IContainer> it = roots.iterator(); it.hasNext();) {
			IResource root = it.next();
			root.accept(new IResourceVisitor() {
				public boolean visit(IResource resource) throws CoreException {
					try {
						if (resource.getType() == IResource.FILE
								&& isCanidate(resource)) {
							buildList.add((IFile) resource);
						}
						return true;
					} catch (Throwable t) {
						Debug.report(t);
						throw new RuntimeException(t);
					}
				}
			});
		}
	}

	/**
	 * @param delta
	 * @param existenceList
	 * @param contentsList
	 * @return
	 * @throws CoreException
	 */
	private void collect(IResourceDelta delta, final Set<IFile> contentsList,
			final Set<IFile> existenceList) throws CoreException {
		final IPrologProject plProject = (IPrologProject) getProject()
				.getNature(PDTCore.NATURE_ID);

		delta.accept(new IResourceDeltaVisitor() {
			public boolean visit(IResourceDelta delta) throws CoreException {
				try {
					IResource resource = delta.getResource();
					boolean isCanidate = isCanidate(resource);
					if (isCanidate && resource.getType() == IResource.FILE) {
						switch (delta.getKind()) {
						case IResourceDelta.ADDED:
						case IResourceDelta.REMOVED:
							existenceList.add((IFile) resource);
						default:
							contentsList.add((IFile) resource);
						}

						return false;
					}
					return isCanidate;
				} catch (Throwable t) {
					Debug.report(t);
					throw new RuntimeException(t);
				}
			}
		});
	}

	private void forget(IFile file, AsyncPrologSession as)
			throws CoreException, PrologInterfaceException {
		file.deleteMarkers(IMarker.PROBLEM, true, 0);

		File ioFile = file.getLocation().toFile();
		String plFileName = Util.prologFileName(ioFile);

		as.queryOnce(file, "pdt_forget_annotation('" + plFileName + "')");

	}


	private void forget(Set<IFile> v, AsyncPrologSession as, IProgressMonitor monitor)
			throws CoreException, PrologInterfaceException {
		for (Iterator<IFile> it = v.iterator(); it.hasNext() && !monitor.isCanceled();) {
			IFile file = (IFile) it.next();
			forget(file, as);
		}
	}

	private boolean isCanidate(IResource r) throws CoreException {
		final IPrologProject plProject = (IPrologProject) getProject()
				.getNature(PDTCore.NATURE_ID);
		if (plProject.isPrologSource(r)) {
			return true;
		}
		Set<IContainer> srcDirs = plProject.getExistingSourcePathEntries();
		for (Iterator<IContainer> it = srcDirs.iterator(); it.hasNext();) {
			IResource srcDir = it.next();
			if (r.getFullPath().isPrefixOf(srcDir.getFullPath())) {
				return true;
			}
		}
		return false;
	}
}

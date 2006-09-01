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
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
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
import org.eclipse.ui.texteditor.MarkerUtilities;

/**
 */
public class PrologBuilder extends IncrementalProjectBuilder {

	/**
	 * 
	 */
	public PrologBuilder() {
		super();
	}

	private void build(IFile file, AsyncPrologSession as) throws CoreException,
			PrologInterfaceException {

		File ioFile = file.getLocation().toFile();
		String plFileName = Util.prologFileName(ioFile);
		
		as.queryOnce(file, "pdt_ensure_annotated('" + plFileName + "')");		
		
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
	 *      java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
	 */
	protected IProject[] build(int kind, Map args,
			final IProgressMonitor monitor) throws CoreException {
		try {

			Debug.debug("PrologBuilder.build(...) was triggered");
			String taskname = "updating prolog metadata";
			;
			Set forgetList = new HashSet();
			Set buildList = new HashSet();
			switch (kind) {
			case IncrementalProjectBuilder.AUTO_BUILD:
			case IncrementalProjectBuilder.INCREMENTAL_BUILD:

				collect(getDelta(getProject()), buildList, forgetList);
				break;
			case IncrementalProjectBuilder.FULL_BUILD:
				getProject().deleteMarkers(IMarker.PROBLEM, true,
						IResource.DEPTH_INFINITE);
				collect(getProject(), buildList);
				break;
			case IncrementalProjectBuilder.CLEAN_BUILD:
				getProject().deleteMarkers(IMarker.PROBLEM, true,
						IResource.DEPTH_INFINITE);
				collect(getProject(), forgetList);
				break;
			default:
				Debug.error("Wasn das f�r ein Buil kind jetzt?");
				return null;
			}
			
			Debug.debug("PrologBuilder.build(...) wants to forget: "
					+ forgetList.toString());
			Debug.debug("PrologBuilder.build(...) wants to build: "
					+ buildList.toString());

			IPrologProject plProject = (IPrologProject) getProject().getNature(
					PDTCore.NATURE_ID);

			final AsyncPrologSession as = ((PrologInterface2) plProject
					.getMetadataPrologInterface()).getAsyncSession();

			monitor.beginTask(taskname, forgetList.size() + buildList.size());
			as.addBatchListener(new DefaultAsyncPrologSessionListener() {
				public void goalSucceeded(AsyncPrologSessionEvent e) {
					if(e.ticket instanceof IFile){
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
				@Override
				public void batchError(AsyncPrologSessionEvent e) {
					//XXX: not sure if this is the right way to do it...
					monitor.setCanceled(true);
					
				}
			});
			try {
				String value = plProject.getPreferenceValue(PDTCore.PROP_PARSE_COMMENTS, "false");
				as.queryOnce("set parse_comments option", "pdt_annotator:pdt_set_preference_value(parse_comments,"+value+")");
				
//				File cacheDir = PDTCorePlugin.getDefault().getStateLocation().append(PDTCore.CACHE_DIR).toFile();
//				String plCacheDir = Util.prologFileName(cacheDir);
//				as.queryOnce("set cache dir option", "pdt_annotator_cache:pdt_set_preference_value(cache_dir,'"+plCacheDir+"')");
				forget(forgetList, as, monitor);
				build(buildList, as, monitor);
				if(!monitor.isCanceled()){
					as.join();
				}
			} finally {
				if (as != null) {
					as.dispose();

				}
			}
			if(monitor.isCanceled()){
				return null;
			}
			PrologSession s = plProject.getMetadataPrologInterface()
					.getSession();
			try {
				update_markers(buildList, s);
				s.queryOnce("pdt_write_cache_index");
				s.queryOnce("pdt_index_save_to_disk");
			} finally {
				if (s != null) {
					s.dispose();
				}
			}
			Debug.debug("PrologBuilder.build(...) is done.");
			monitor.done();
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

	private void update_markers(Set buildList, PrologSession s)
			throws CoreException, PrologException, PrologInterfaceException, IOException {
		StringBuffer sb = new StringBuffer();
		sb.append('[');
		boolean first = true;
		Map wsFiles = new HashMap();
		Map fileContents = new HashMap(); //plFile as key
		for (Iterator it = buildList.iterator(); it.hasNext();) {
			if (!first) {
				sb.append(',');
			}
			IFile file = (IFile) it.next();
			file.deleteMarkers(IMarker.PROBLEM, true, 0);
			File ioFile = file.getLocation().toFile();
			String plFileName = Util.prologFileName(ioFile);
			wsFiles.put(plFileName, file);
			InputStream stream = file.getContents();
			String data=Util.toString(stream);
			stream.close();
			fileContents.put(plFileName, data);
			sb.append('\'');
			sb.append(plFileName);
			sb.append('\'');
			first = false;
		}
		sb.append(']');
		String fileList = sb.toString();
		List list = s.queryAll("member(File," + fileList
				+ "),pdt_file_error(File,"
				+ "error(Type, stream(_, Line, Column, CharOffset))),"
				+ "message_to_string(error(Type, _),Message)");
		for (Iterator it = list.iterator(); it.hasNext();) {
			Map map = (Map) it.next();
			String plFile = (String) map.get("File");
			IFile wsFile = (IFile) wsFiles.get(plFile);
			String data = (String) fileContents.get(plFile);
			int line = Integer.parseInt((String) map.get("Line"));
			int column = Integer.parseInt((String) map.get("Column"));
			int offset = PDTCoreUtils.convertCharacterOffset(data, Integer.parseInt((String) map.get("CharOffset")));
			String message = (String) map.get("Message");
			
			IMarker marker = wsFile.createMarker(IMarker.PROBLEM);
			HashMap attributes = new HashMap();
			MarkerUtilities.setMessage(attributes, message);
			MarkerUtilities.setLineNumber(attributes, line);

			MarkerUtilities.setCharStart(attributes, offset);
			MarkerUtilities.setCharEnd(attributes, offset + 1);

			attributes.put(IMarker.SEVERITY,
					new Integer(IMarker.SEVERITY_ERROR));
			marker.setAttributes(attributes);
		}

		list = s.queryAll("member(File," + fileList
				+ "),pdt_file_problem(File," + "error(Error),From-To)");
		for (Iterator it = list.iterator(); it.hasNext();) {
			Map map = (Map) it.next();
			String plFile = (String) map.get("File");
			IFile wsFile = (IFile) wsFiles.get(plFile);
			String data = (String) fileContents.get(plFile);
			int start = PDTCoreUtils.convertCharacterOffset(data,Integer.parseInt((String) map.get("From")));
			int end = PDTCoreUtils.convertCharacterOffset(data,Integer.parseInt((String) map.get("To")));
			String message = getMessage((String) map.get("Error"));
			IMarker marker = wsFile.createMarker(IMarker.PROBLEM);
			HashMap attributes = new HashMap();
			MarkerUtilities.setMessage(attributes, message);

			MarkerUtilities.setCharStart(attributes, start);
			MarkerUtilities.setCharEnd(attributes, end);

			attributes.put(IMarker.SEVERITY,
					new Integer(IMarker.SEVERITY_ERROR));
			marker.setAttributes(attributes);
		}

		list = s.queryAll("member(File," + fileList
				+ "),pdt_file_problem(File," + "warning(Warning),From-To)");
		for (Iterator it = list.iterator(); it.hasNext();) {
			Map map = (Map) it.next();
			String plFile = (String) map.get("File");
			IFile wsFile = (IFile) wsFiles.get(plFile);
			String data = (String) fileContents.get(plFile);
			int start = PDTCoreUtils.convertCharacterOffset(data,Integer.parseInt((String) map.get("From")));
			int end = PDTCoreUtils.convertCharacterOffset(data,Integer.parseInt((String) map.get("To")));
			
			String message = getMessage((String) map.get("Warning"));
			IMarker marker = wsFile.createMarker(IMarker.PROBLEM);
			HashMap attributes = new HashMap();
			MarkerUtilities.setMessage(attributes, message);

			MarkerUtilities.setCharStart(attributes, start);
			MarkerUtilities.setCharEnd(attributes, end);

			attributes.put(IMarker.SEVERITY,
					new Integer(IMarker.SEVERITY_WARNING));
			marker.setAttributes(attributes);
		}
	}

	private String getMessage(String string) {
		if(string.startsWith("singleton(")){
			String varname = string.substring("singleton(".length(), string.length()-1);
			return "[singleton] Variable "+varname+" apears only once in clause.";
		}
		if("no_singleton".equals(string)){
			return "[no_singleton] Variable is not a singleton.";
		}
		if("malformed_export".equals(string)){
			return "[malformed_export] Malformed signature in export list.";
		}
		if("undefined_export".equals(string)){
			return "[undefined_export] Predicate is exported but not defined by the module.";
		}		
		if("malformed_signature".equals(string)){
			return "[malformed_signature] Malformed signature in property definition.";
		}
		
		return "[unknown error] "+string;
	}

	/**
	 * @param v
	 * @throws IOException
	 * @throws CoreException
	 * @throws PrologInterfaceException
	 */
	private void build(Set v, AsyncPrologSession as, IProgressMonitor monitor)
			throws CoreException, IOException, PrologInterfaceException {

		for (Iterator it = v.iterator(); !monitor.isCanceled() && it.hasNext();) {
			IFile file = (IFile) it.next();
			build(file, as);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IncrementalProjectBuilder#clean(org.eclipse.core.runtime.IProgressMonitor)
	 */
	protected void clean(IProgressMonitor monitor) throws CoreException {
		Set forgetList = new HashSet();
		collect(getProject(), forgetList);
		getProject().deleteMarkers(IMarker.PROBLEM, true,
				IResource.DEPTH_INFINITE);
		IPrologProject plProject = (IPrologProject) getProject().getNature(
				PDTCore.NATURE_ID);
		AsyncPrologSession as = null;
		try {
			as = ((PrologInterface2) plProject.getMetadataPrologInterface())
					.getAsyncSession();

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
	private void collect(IProject project, final Set buildList)
			throws CoreException {
		final IPrologProject plProject = (IPrologProject) project
				.getNature(PDTCore.NATURE_ID);
		Set roots = plProject.getExistingSourcePathEntries();
		for (Iterator it = roots.iterator(); it.hasNext();) {
			IResource root = (IResource) it.next();
			root.accept(new IResourceVisitor() {
				public boolean visit(IResource resource) throws CoreException {
					try {
						if (resource.getType() == IResource.FILE
								&& isCanidate(resource)) {
							buildList.add(resource);
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
	 * @param forgetList
	 * @param buildList
	 * @return
	 * @throws CoreException
	 */
	private void collect(IResourceDelta delta, final Set buildList,
			final Set forgetList) throws CoreException {
		final IPrologProject plProject = (IPrologProject) getProject()
				.getNature(PDTCore.NATURE_ID);

		delta.accept(new IResourceDeltaVisitor() {
			public boolean visit(IResourceDelta delta) throws CoreException {
				try {
					IResource resource = delta.getResource();
					boolean isCanidate = isCanidate(resource);
					if (isCanidate && resource.getType() == IResource.FILE) {
						if (delta.getKind() == IResourceDelta.REMOVED) {
							forgetList.add(resource);
						} else {
							buildList.add(resource);
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

	/**
	 * @param as
	 * @param monitor
	 * @param forgetList
	 * @throws CoreException
	 * @throws PrologInterfaceException
	 */
	private void forget(Set v, AsyncPrologSession as, IProgressMonitor monitor)
			throws CoreException, PrologInterfaceException {
		for (Iterator it = v.iterator(); it.hasNext()&& ! monitor.isCanceled();) {
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
		Set srcDirs = plProject.getExistingSourcePathEntries();
		for (Iterator it = srcDirs.iterator(); it.hasNext();) {
			IResource srcDir = (IResource) it.next();
			if (r.getFullPath().isPrefixOf(srcDir.getFullPath())) {
				return true;
			}
		}
		return false;
	}
}

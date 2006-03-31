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

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.parser.PrologCompilerFactory;
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
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.SubProgressMonitor;

/**
 */
public class MetaDataBuilder extends IncrementalProjectBuilder {

	/**
	 * 
	 */
	public MetaDataBuilder() {
		super();
	}

	private void build(IFile file, PrintStream outputStream)
			throws CoreException, IOException {

		IFileLineBreakInfoProvider lineInfo = new IFileLineBreakInfoProvider(
				file);
		MarkerProblemCollector collector = new MarkerProblemCollector(file,
				lineInfo);
		MarkerTaskCollector taskCollector = new MarkerTaskCollector(file);
		final String fileName = file.getFullPath().toString();

		PrologCompiler checker = PrologCompilerFactory.create();
		checker.setProblemCollector(collector);
		checker.setTaskCollector(taskCollector);
		checker.compile(fileName, file.getContents(), lineInfo);
		// PDTPlugin plugin = PDTPlugin.getDefault();
		

		checker.saveMetaDataForClauses(outputStream);
		checker.saveAbbaData(new BufferedOutputStream(outputStream));

		
	}

		/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
	 *      java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
	 */
	protected IProject[] build(int kind, Map args, IProgressMonitor monitor)
			throws CoreException {
		try {
			String val = PDTCorePlugin.getDefault().getPreferenceValue(PDTCore.PREF_PARSER,PDTCore.JAVACC);
			if(!PDTCore.JAVACC.equals(val)){
				Debug.info("skipping MetaDataBuilder, parser framework is set to "+val);
				return null;
			}
			Debug.debug("MetaDataBuilder.build(...) was triggered");
			String taskname = "building prolog metadata";
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
			forgetList.addAll(buildList);
			Debug.debug("MetaDataBuilder.build(...) wants to forget: "
					+ forgetList.toString());
			Debug.debug("MetaDataBuilder.build(...) wants to build: "
					+ buildList.toString());
			monitor.beginTask(taskname, forgetList.size() + buildList.size());
			IPrologProject plProject = (IPrologProject) getProject().getNature(
					PDTCore.NATURE_ID);
			
			/* idea: buffer everything and consult it in one big burst.
			 * TODO: need to limit buffer size.
			 * bursting is probably a good idea, but if memory consumption 
			 * causes to many page swaps, this may even slow things down.
			 */
			
			
			ByteArrayOutputStream buffStream = new ByteArrayOutputStream();
			PrintStream out=new PrintStream(buffStream);
			// PrintStream out = new PrintStream(new
			// FileOutputStream("c:\\temp\\consulted.pl"));
			
				forget(forgetList, out, new SubProgressMonitor(monitor,
						forgetList.size()));
				build(buildList, out, new SubProgressMonitor(monitor, buildList
						.size()));
			PrintStream realout = plProject.getMetadataPrologInterface().getConsultService(
						PDTCore.CS_METADATA).getOutputStream("flat_pl_metadata.pl");
			try {
				realout.write(buffStream.toByteArray());
			} finally {
				realout.close();
			}
			Debug.debug("MetaDataBuilder.build(...) is done.");
			monitor.done();
			return null;
		} catch (OperationCanceledException e) {
			throw e;
		} catch (Throwable t) {
			Debug.report(t);
			return null;
		}
	}

	/**
	 * @param v
	 * @param monitor
	 * @throws IOException
	 * @throws CoreException
	 */
	private void build(Set v, PrintStream out, IProgressMonitor monitor)
			throws CoreException, IOException {
		monitor.beginTask("building prolog metadata", v.size());
		final HashMap timings = new HashMap();
		String[] filenames = new String[v.size()];
		int i = 0;
		for (Iterator it = v.iterator(); it.hasNext();) {
			IFile file = (IFile) it.next();
			filenames[i] = file.getFullPath().toOSString();

			try {
				long time = System.currentTimeMillis();
				build(file, out);
				time = System.currentTimeMillis() - time;
				timings.put(filenames[i++], new Long(time));
			} catch (Throwable e) {
				timings.put(filenames[i++], new Long(Long.MAX_VALUE));
			}

			monitor.worked(1);
		}
		Arrays.sort(filenames, new Comparator() {

			public int compare(Object o1, Object o2) {
				Long l1 = (Long) timings.get(o1);
				Long l2 = (Long) timings.get(o2);
				return -l1.compareTo(l2);
			}

		});
		Debug.info("TOP 20 Build Times:");
		StringBuffer sb = new StringBuffer();
		for (int j = 0; j < Math.min(20, filenames.length); j++) {
			String filename = filenames[j];
			sb.append("\n");
			sb.append("" + j + " " + filename + ":  " + timings.get(filename));
		}
		Debug.info(sb.toString());
		monitor.done();
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
		PrintStream out = plProject.getMetadataPrologInterface().getConsultService(
				PDTCore.CS_METADATA).getOutputStream("flat_pl_metadata.pl");
		try {
			forget(forgetList, out, monitor);
		} finally {
			out.close();
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

	private void forget(IFile file, PrintStream out) throws CoreException {
		file.deleteMarkers(IMarker.PROBLEM, true, 0);
		String s = file.getFullPath().toString();
		out.println(":- retractall(meta_data_module('" + s + "',_,_)).");
		out.println(":- retractall(meta_data('" + s + "',_,_,_,_,_,_,_,_)).");

	}

	/**
	 * @param out
	 * @param forgetList
	 * @param monitor
	 * @throws CoreException
	 */
	private void forget(Set v, PrintStream out, IProgressMonitor monitor)
			throws CoreException {
		monitor.beginTask("forgetting prolog metadata", v.size());
		for (Iterator it = v.iterator(); it.hasNext();) {
			IFile file = (IFile) it.next();
			forget(file, out);
			monitor.worked(1);
		}
		monitor.done();

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

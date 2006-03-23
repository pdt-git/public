/*
 */
package org.cs3.pdt.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.PrologInterface2;
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

	private void build(IFile file, AsyncPrologSession as)
			throws CoreException, IOException {

		
		File ioFile = file.getLocation().toFile();
		String plFileName =	Util.prologFileName(ioFile);
		
		as.queryOnce(file,"ensure_annotated('"+plFileName+"')");
		
	}

		/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
	 *      java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
	 */
	protected IProject[] build(int kind, Map args, final IProgressMonitor monitor)
			throws CoreException {
		try {
			String val = PDTCorePlugin.getDefault().getPreferenceValue(PDTCore.PREF_PARSER,PDTCore.JAVACC);
			if(!PDTCore.READ_TERM_3.equals(val)){
				Debug.info("skipping MetaDataBuilder, parser framework is set to "+val);
				return null;
			}
			Debug.debug("PrologBuilder.build(...) was triggered");
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
			Debug.debug("PrologBuilder.build(...) wants to forget: "
					+ forgetList.toString());
			Debug.debug("PrologBuilder.build(...) wants to build: "
					+ buildList.toString());
			
			IPrologProject plProject = (IPrologProject) getProject().getNature(
					PDTCore.NATURE_ID);
			

			final AsyncPrologSession as = ((PrologInterface2)plProject.getMetadataPrologInterface()).getAsyncSession();
			
			monitor.beginTask(taskname, forgetList.size() + buildList.size());
			as.addBatchListener(new DefaultAsyncPrologSessionListener(){
				public void goalSucceeded(AsyncPrologSessionEvent e) {
					monitor.worked(1);
					if(monitor.isCanceled()){
						new Thread(){
							public void run() {
								as.abort();
								Debug.debug("prolog builder aborted (theoretically)");
							}
						}.start();
						
					}
				}
			});
			try{
				forget(forgetList, as,monitor);			
				build(buildList,as,monitor);
			}finally{
				if(as!=null){
					as.dispose();
					
				}
			}

			PrologSession s = plProject.getMetadataPrologInterface().getSession();
			try{
				update_markers(buildList,s);
			} finally{
				if(s!=null){
					s.dispose();
				}
			}
			Debug.debug("PrologBuilder.build(...) is done.");
			monitor.done();
			return null;
		} catch (OperationCanceledException e) {
			throw e;
		} catch (Throwable t) {
			Debug.report(t);
			return null;
		}
	}

	private void update_markers(Set buildList, PrologSession s) throws CoreException {
		StringBuffer sb = new StringBuffer();
		sb.append('[');
		boolean first=true;
		Map wsFiles = new HashMap();
		for (Iterator it = buildList.iterator(); it.hasNext();) {
			if(!first){
				sb.append(',');
			}
			IFile file = (IFile) it.next();
			file.deleteMarkers(IMarker.PROBLEM, true, 0);
			File ioFile = file.getLocation().toFile();
			String plFileName =	Util.prologFileName(ioFile);
			wsFiles.put(plFileName, file);
			sb.append('\'');
			sb.append(plFileName);
			sb.append('\'');
			first=false;
		}
		sb.append(']');
		String fileList = sb.toString();
		List list = s.queryAll("member(File,"+fileList+"),current_file_error(File," +
				"error(Type, stream(_, Line, Column, CharOffset)))," +
				"message_to_string(error(Type, _),Message)");
		for (Iterator it = list.iterator(); it.hasNext();) {
			Map map = (Map) it.next();
			String plFile = (String) map.get("File");
			int line = Integer.parseInt((String) map.get("Line"));
			int column = Integer.parseInt((String) map.get("Column"));
			int offset = Integer.parseInt((String) map.get("CharOffset"));
			String message = (String)map.get("Message");
			IFile wsFile = (IFile) wsFiles.get(plFile);
			IMarker marker = wsFile.createMarker(IMarker.PROBLEM);
			HashMap attributes = new HashMap();
	        MarkerUtilities.setMessage(attributes, message);
	        MarkerUtilities.setLineNumber(attributes, line);
	        
	        MarkerUtilities.setCharStart(attributes, offset);
	        MarkerUtilities.setCharEnd(attributes, offset+1);
	        
	        
	        attributes.put(IMarker.SEVERITY, new Integer(IMarker.SEVERITY_ERROR));
	        	marker.setAttributes(attributes);
		}
	}

	/**
	 * @param v
	 * @throws IOException
	 * @throws CoreException
	 */
	private void build(Set v, AsyncPrologSession as,IProgressMonitor monitor)
			throws CoreException, IOException {
		
		
		for (Iterator it = v.iterator(); !monitor.isCanceled()&&it.hasNext();) {
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
		AsyncPrologSession as = ((PrologInterface2)plProject.getMetadataPrologInterface()).getAsyncSession();
		try {
			forget(forgetList, as,monitor);
		} finally {
			if(as!=null){
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

	private void forget(IFile file, AsyncPrologSession as) throws CoreException {
		file.deleteMarkers(IMarker.PROBLEM, true, 0);
		
		File ioFile = file.getLocation().toFile();
		String plFileName =	Util.prologFileName(ioFile);
		
		as.queryOnce(file, "forget_file_annotation('"+plFileName+"')");
		

	}

	/**
	 * @param as
	 * @param monitor 
	 * @param forgetList
	 * @throws CoreException
	 */
	private void forget(Set v, AsyncPrologSession as, IProgressMonitor monitor)
			throws CoreException {
		for (Iterator it = v.iterator(); it.hasNext();) {
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

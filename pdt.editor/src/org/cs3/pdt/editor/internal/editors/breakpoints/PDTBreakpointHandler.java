/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.editors.breakpoints;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.service.ActivePrologInterfaceListener;
import org.cs3.pdt.connector.service.ConsultListener;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.PDTPredicates;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.common.logging.Debug;
import org.cs3.prolog.connector.lifecycle.LifeCycleHook;
import org.cs3.prolog.connector.lifecycle.PrologEventDispatcher;
import org.cs3.prolog.connector.process.PrologInterface;
import org.cs3.prolog.connector.process.PrologInterfaceEvent;
import org.cs3.prolog.connector.process.PrologInterfaceException;
import org.cs3.prolog.connector.process.PrologInterfaceListener;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class PDTBreakpointHandler implements PrologInterfaceListener, LifeCycleHook, ActivePrologInterfaceListener, ConsultListener {

	private static final String ADD_BREAKPOINT = "add_breakpoint";
	private static final String REMOVE_BREAKPOINT = "remove_breakpoint";
	private static final String BREAKPOINT_LIFECYCLE_HOOK = "BreakpointLifecycleHook";
	private static final String[] DEPENDENCIES = {PDTCommonPlugin.LIFE_CYCLE_HOOK_ID};
	private static final String SOURCE_FILE = "source_file";
	private static final String DELETE_BREAKPOINT = "delete_breakpoint";
	private static final String BREAKPOINT_PROPERTY = "pdt_editor_breakpoints:pdt_breakpoint_property";

	private static final String PDT_BREAKPOINT_MARKER = "org.cs3.pdt.editor.editor.PDTBreakpointMarker";
	private static final String BREAKPOINT_ID = "pdt.breakpoint.id";
	private static final String BREAKPOINT_OFFSET = "pdt.breakpoint.offset";
	private static final String BREAKPOINT_OFFSET_PHYSICAL = "pdt.breakpoint.offset.physical";

	private static PDTBreakpointHandler instance;

	private PrologInterface currentPif = null;
	private PrologEventDispatcher currentDispatcher;
	private Set<String> deletedIds;

	public static PDTBreakpointHandler getInstance() {
		if (instance == null)
			instance = new PDTBreakpointHandler();
		return instance;
	}

	private PDTBreakpointHandler() {
		checkForPif();
		PDTConnectorPlugin.getDefault().getPrologInterfaceService().registerActivePrologInterfaceListener(this);
		PDTConnectorPlugin.getDefault().getPrologInterfaceService().registerConsultListener(this);
	}

	private void checkForPif() {
		if (currentPif == null) {
			currentPif = PDTCommonUtil.getActivePrologInterface();
			addPifListener();
		}
	}


	List<MarkerBackup> markerBackup = null;

	public void backupMarkers() {
		markerBackup = new ArrayList<MarkerBackup>();

		try {
			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
			for (IMarker marker : markers) {
				int line = marker.getAttribute(IMarker.LINE_NUMBER, 0);
				int currentOffset = marker.getAttribute(IMarker.CHAR_START, -1);
				int offsetWhenSetting = marker.getAttribute(BREAKPOINT_OFFSET_PHYSICAL, -1);
				int logicOffset = marker.getAttribute(BREAKPOINT_OFFSET, -1);
				String id = marker.getAttribute(BREAKPOINT_ID, "");
				if (currentOffset == -1 || offsetWhenSetting == -1 || currentOffset != offsetWhenSetting) {
					markerBackup.add(new MarkerBackup(marker.getResource(), line, id, -1));
				} else {
					markerBackup.add(new MarkerBackup(marker.getResource(), line, id, logicOffset));	
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}

		// enable logging of deleted ids
		deletedIds = new HashSet<String>();
	}

	public boolean shouldUpdateMarkers = true;
	
	public void updateMarkers() {
			if (!shouldUpdateMarkers || markerBackup == null || deletedIds == null) {
				return;
			}
			
			for (final MarkerBackup m : markerBackup) {
				
				runAsJob("recreate marker", m.getFile(), new Runnable() {
					
					@Override
					public void run() {
						// only recreate the marker if it was deleted
						if (deletedIds.contains(m.getId())) {
							int offset = m.getOffset();
							try {
								if (offset == -1) {
									IDocument document2;
									document2 = UIUtils.getDocument(m.getFile());
									offset = UIUtils.physicalToLogicalOffset(document2, document2.getLineInformation(m.getLineNumber() - 1).getOffset());
								}
								try {
									currentPif.queryOnce(bT(PDTPredicates.PDT_SET_BREAKPOINT, getPrologFileName(m.getFile()), m.getLineNumber(), offset, "Id"));
								} catch (PrologInterfaceException e) {
									Debug.report(e);
								}
							} catch (CoreException e) {
								Debug.report(e);
							} catch (BadLocationException e) {
								Debug.report(e);
							}
						}						
					}
				});
				
				
			}

			runAsJob("recreate marker", ResourcesPlugin.getWorkspace().getRoot(), new Runnable() {
				
				@Override
				public void run() {
					// disable logging of deleted ids
					deletedIds = null;
					markerBackup = null;
				}
			});
	}

	private String getPrologFileName(IFile file) {
		String enclFile = file.getRawLocation().toPortableString();
		if (Util.isWindows()) {
			enclFile = enclFile.toLowerCase();
		}

		IPath filepath = new Path(enclFile);
		return "'" + Util.prologFileName(filepath.toFile()) + "'";
	}


	private IMarker getBreakpointAtLine(IFile file, int line) {
		// used to check if there is an existing breakpoint in this line
		// used by toggleBreakpoint to see if the marker has to be added or removed
		if (file != null) {
			try {
				IMarker[] markers = file.findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
				for (IMarker marker : markers) {
					if (marker.getAttribute(IMarker.LINE_NUMBER, 0) == line)
						return marker;
				}
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
		return null;
	}

	private void addMarker(String fileName, int line, int offset, int length, String id) throws CoreException {
		// called when a breakpoint was added in prolog
		try {
			IFile file = FileUtils.findFileForLocation(fileName);
			addMarker(file, line, offset, length, id);
		} catch (IOException e) {
			Debug.report(e);
		} catch (IllegalArgumentException e) {
			if (e.getMessage().startsWith("Not in Workspace: ")) {
				Debug.warning("Try to set breakpoint marker in non-workspace file");
			} else {
				Debug.report(e);
			}
		}
	}

	private void addMarker(final IFile file, final int line, final int offset, final int length, final String id) throws CoreException {
		runAsJob("Add breakpoint", ResourcesPlugin.getWorkspace().getRoot(), new Runnable() {

			@Override
			public void run() {
				HashMap<String, Comparable<?>> attributes = new HashMap<String, Comparable<?>>();
				attributes.put(IMarker.LINE_NUMBER, line);
				attributes.put(IMarker.MESSAGE, "Prolog Breakpoint: line[" + line + "]");
				attributes.put(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
				attributes.put(BREAKPOINT_ID, id);
				attributes.put(BREAKPOINT_OFFSET, offset);
				try {
					IDocument document2 = UIUtils.getDocument(file);
					int start = UIUtils.logicalToPhysicalOffset(document2, offset);
					int end = UIUtils.logicalToPhysicalOffset(document2, offset + length);
					attributes.put(BREAKPOINT_OFFSET_PHYSICAL, start);
					attributes.put(IMarker.CHAR_START, start);
					attributes.put(IMarker.CHAR_END, end);
				} catch (CoreException e) {
					Debug.report(e);
				}
				try {
					MarkerUtilities.createMarker(file, attributes, PDT_BREAKPOINT_MARKER);
				} catch (CoreException e) {
					Debug.report(e);
				}
			}
		});
	}

	private void removeAllBreakpointMarkers() {
		runAsJob("Remove all breakpoints", ResourcesPlugin.getWorkspace().getRoot(), new Runnable() {
			
			@Override
			public void run() {
				// called when active prolog interface was changed
				try {
					ResourcesPlugin.getWorkspace().getRoot().deleteMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
					//			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
					//			for (IMarker marker : markers) {
					//				marker.delete();
					//			}
				} catch (CoreException e) {
					Debug.report(e);
				}
				
			}
		});
	}
	
	private void runAsJob(String name, ISchedulingRule rule, final Runnable runnable) {
		Job job = new Job(name) {
			
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				runnable.run();
				return Status.OK_STATUS;
			}
		};
		job.setRule(rule);
		job.schedule();
	}

	public void removeBreakpointFactsForFile(String prologFileName) {
		checkForPif();
		try {
			List<Map<String, Object>> results = currentPif.queryAll(bT(BREAKPOINT_PROPERTY, "Id" , "file('" + prologFileName + "')"));
			for (Map<String, Object> r : results) {
				currentPif.queryOnce(bT(DELETE_BREAKPOINT, r.get("Id")));
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		}
	}

	public void toogleBreakpoint(IFile file, int line, int offset) {
		if (file == null) {
			UIUtils.displayErrorDialog(Display.getCurrent().getActiveShell(), "File is not in workspace", "You can only set breakpoint markers to files, which are in your workspace.");
			return;
		}
		checkForPif();
		String prologFileName = getPrologFileName(file);
		if (line > 0) {
			IMarker existingMarker = getBreakpointAtLine(file, line);
			if (existingMarker == null) {
				// add marker
				if (file != null) {
					try {
						boolean isSourceFile = (currentPif.queryOnce(bT(SOURCE_FILE, prologFileName)) != null);
						if (isSourceFile) {
							executeSetBreakpointQuery(prologFileName, line, offset);
						} else {
							UIUtils.displayErrorDialog(Display.getCurrent().getActiveShell(), "File is not loaded", "You are trying to set a breakpoint to a file which is not loaded. You have to consult the file before you can set a breakpoint.");
						}
					} catch (PrologInterfaceException e) {
						Debug.report(e);
					}
				}
			} else {
				// remove marker
				try {
					String id = existingMarker.getAttribute(BREAKPOINT_ID, "");
					if (!id.isEmpty()) {
						currentPif.queryOnce(bT(BREAKPOINT_PROPERTY, id, "file(_)"),
								bT(DELETE_BREAKPOINT, id));
					}
					// if for some strange reason the marker is still there, even if there is no
					// breakpoint in prolog, we have to delete the marker here manually
					existingMarker.delete();
				} catch (CoreException e) {
					Debug.report(e);				
				} catch (PrologInterfaceException e) {
					Debug.report(e);				
				}
			}
		}		
	}

	public void executeSetBreakpointQuery(String prologFileName, int line, int offset) throws PrologInterfaceException {
		Debug.debug("Set breakpoint in file " + prologFileName + " (line: " + line + ", offset: " + offset + ")");
		String query = bT(PDTPredicates.PDT_SET_BREAKPOINT, prologFileName, line, offset, "_");
		PrologInterface pif = PDTCommonUtil.getActivePrologInterface();
		pif.queryOnce(query);
	}

	private void loadBreakpointsFromPif() {
		List<Map<String, Object>> results;
		try {
			results = currentPif.queryAll(bT(BREAKPOINT_PROPERTY, "Id" , "file(File)"),
					bT(BREAKPOINT_PROPERTY, "Id" , "line_count(Line)"),
					bT(BREAKPOINT_PROPERTY, "Id" , "character_range(Offset, Length)"));
			for (Map<String, Object> result : results) {
				int line = Integer.parseInt(result.get("Line").toString());
				int offset = Integer.parseInt(result.get("Offset").toString());
				int length = Integer.parseInt(result.get("Length").toString());
				String id = result.get("Id").toString();
				try {
					addMarker(result.get("File").toString(), line, offset, length, id);
				} catch (IllegalArgumentException e) {
					Debug.report(e);
				}
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		} catch (CoreException e) {
			Debug.report(e);
		}


	}

	private void addPifListener() {
		if (currentPif != null) {
			Debug.debug("add listener for pif " + currentPif.toString());

			currentDispatcher = new PrologEventDispatcher(currentPif);
			currentPif.addLifeCycleHook(this, BREAKPOINT_LIFECYCLE_HOOK, DEPENDENCIES);
			try {
				currentDispatcher.addPrologInterfaceListener(ADD_BREAKPOINT, this);
				currentDispatcher.addPrologInterfaceListener(REMOVE_BREAKPOINT, this);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}

		}
	}

	private void removePifListener() {
		if (currentPif != null && currentDispatcher != null) {
			Debug.debug("remove listener for pif " + currentPif.toString());
			currentPif.removeLifeCycleHook(BREAKPOINT_LIFECYCLE_HOOK);
			try {
				currentDispatcher.removePrologInterfaceListener(ADD_BREAKPOINT, this);
				currentDispatcher.removePrologInterfaceListener(REMOVE_BREAKPOINT, this);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
		}
	}

	@Override
	public void update(PrologInterfaceEvent e) {
		if (e.getSubject().equals(ADD_BREAKPOINT)) {
			String id = e.getEvent();
			try {
				Map<String, Object> result = currentPif.queryOnce(bT(PDTPredicates.PDT_BREAKPOINT_PROPERTIES, id, "File", "Line", "Offset", "Length"));
				String file = result.get("File").toString();
				int line = Integer.parseInt(result.get("Line").toString());
				int offset = Integer.parseInt(result.get("Offset").toString());
				int length = Integer.parseInt(result.get("Length").toString());
				addMarker(file, line, offset, length, id);
			} catch (PrologInterfaceException e1) {
				Debug.report(e1);
			} catch (CoreException e1) {
				Debug.report(e1);
			}
		} else if (e.getSubject().equals(REMOVE_BREAKPOINT)) {
			String id = e.getEvent();
			removeMarkerWithId(id);
		}
	}

	private void removeMarkerWithId(final String id) {
		try {
			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
			for (IMarker marker : markers) {
				if (marker.getAttribute(BREAKPOINT_ID, "").equals(id)) {
					final IMarker m = marker;
					runAsJob("Delete marker", marker.getResource(), new Runnable() {

						@Override
						public void run() {
							try {
								m.delete();
							} catch (CoreException e) {
								Debug.report(e);
							}
							Debug.debug("remove marker " + id);
							if (deletedIds != null) {
								deletedIds.add(id);
							}
						}
					});
					return;
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}

	@Override
	public void onInit(PrologInterface pif, PrologSession initSession) throws PrologInterfaceException {
	}

	@Override
	public void afterInit(final PrologInterface pif) throws PrologInterfaceException {
		shouldUpdateMarkers = true;
		if (markerBackup == null || markerBackup.isEmpty()) {
			markerBackup = null;
			return;
		}
		Thread t = new Thread(new Runnable() {
			@Override
			public void run() {
				waitForDispatcherSubjectActive();
				StringBuffer buf = new StringBuffer();
				boolean first = true;
				for (MarkerBackup m : markerBackup) {
					// TODO: Debug here, timing issues
					if (first) {
						first = false;
					} else {
						buf.append(", ");
					}
					buf.append(bT(PDTPredicates.PDT_SET_BREAKPOINT, getPrologFileName(m.getFile()), m.getLineNumber(), m.getOffset(), "_"));
					//			executeSetBreakpointQuery(getPrologFileName(m.getFile()), m.getLineNumber(), m.getOffset());
				}
				Debug.debug("Resetting breakpoints after restart: " + buf.toString());
//				PrologInterface pif = PDTCommonUtil.getActivePrologInterface();
				try {
					pif.queryOnce(buf.toString());
				} catch (PrologInterfaceException e) {
					Debug.report(e);
				}
				
				// disable logging of deleted ids
				markerBackup = null;
				shouldUpdateMarkers = true;
			}
		});
		t.start();
	}

	@Override
	public void beforeShutdown(PrologInterface pif, PrologSession session) throws PrologInterfaceException {
		if (currentPif.equals(pif)) {
			shouldUpdateMarkers = false;
			backupMarkers();
			removeAllBreakpointMarkers();
		}
	}

	@Override
	public void onError(PrologInterface pif) {
		if (currentPif.equals(pif)) {
			shouldUpdateMarkers = false;
			backupMarkers();
		}
	}

	@Override
	public void setData(Object data) {}

	@Override
	public void lateInit(PrologInterface pif) {}

	private void waitForDispatcherSubjectActive() {
		PrologEventDispatcher dispatcher = currentDispatcher;
		for (int i = 0; i < 10; i++) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
			}
			if (dispatcher.getSubjects().contains(ADD_BREAKPOINT)) {
				return;
			}
		}
	}

	@Override
	public void activePrologInterfaceChanged(PrologInterface pif) {
		if (currentPif == pif) {
			return;
		}
		
		removePifListener();

		currentPif = pif;
		removeAllBreakpointMarkers();
		loadBreakpointsFromPif();

		addPifListener();
	}

	@Override
	public void beforeConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		if (pif.equals(currentPif) && markerBackup == null) {
			backupMarkers();
		}
	}

	@Override
	public void afterConsult(PrologInterface pif, List<IFile> files, List<String> allConsultedFiles, IProgressMonitor monitor) throws PrologInterfaceException {
		if (pif.equals(currentPif)) {
			Debug.debug("update marker");
			updateMarkers();
		}
		monitor.done();
	}

}



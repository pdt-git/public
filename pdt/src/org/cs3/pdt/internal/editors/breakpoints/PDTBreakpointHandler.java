package org.cs3.pdt.internal.editors.breakpoints;

import static org.cs3.pl.prolog.QueryUtils.bT;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.actions.ConsultActionDelegate;
import org.cs3.pdt.internal.actions.QueryConsoleThreadAction;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.console.prolog.PrologConsoleEvent;
import org.cs3.pl.console.prolog.PrologConsoleListener;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologEventDispatcher;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class PDTBreakpointHandler implements PrologConsoleListener, PrologInterfaceListener, LifeCycleHook {

	private static final String BREAKPOINT_LIFECYCLE_HOOK = "BreakpointLifecycleHook";
	private static final String SOURCE_FILE = "source_file";
	private static final String SET_BREAKPOINT = "pdt_set_breakpoint";
	private static final String DELETE_BREAKPOINT = "delete_breakpoint";
	private static final String BREAKPOINT_PROPERTY = "breakpoint_property";
	
	private static final String PDT_BREAKPOINT_MARKER = "org.cs3.pdt.PDTBreakpointMarker";
	private static final String BREAKPOINT_ID = "pdt.breakpoint.id";
	private static final String BREAKPOINT_OFFSET = "pdt.breakpoint.offset";
	
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
		PrologConsolePlugin.getDefault().getPrologConsoleService().addPrologConsoleListener(this);
		checkForPif();
	}
	
	private void checkForPif() {
		if (currentPif == null) {
			currentPif = PDTUtils.getActiveConsolePif();
			addPifListener();
		}
	}


	List<MarkerBackup> markerBackup = null;
	IFile currentIFile;
	Document document;
	
	public void backupMarkers(IFile currentIFile, Document document) {
		this.currentIFile = currentIFile;
		this.document = document;
		
		markerBackup = new ArrayList<MarkerBackup>();

		try {
			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
			for (IMarker marker : markers) {
				int line = marker.getAttribute(IMarker.LINE_NUMBER, 0);
				int offset = marker.getAttribute(BREAKPOINT_OFFSET, 0);
				String id = marker.getAttribute(BREAKPOINT_ID, "");
				markerBackup.add(new MarkerBackup(marker.getResource(), line, id, offset));	
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
		
		// enable logging of deleted ids
		deletedIds = new HashSet<String>();
	}

	public void updateMarkers() {
		if (markerBackup == null) {
			return;
		}
		
		for (MarkerBackup m : markerBackup) {
			// only recreate the marker if it was deleted
			if (deletedIds.contains(m.getId())) {
				if (m.getFile().equals(currentIFile)) {
					// if the marker is in the current file, recreate information from the document (the marker could be moved)
					try {
						int offset = document.getLineInformation(m.getLineNumber() - 1).getOffset();
						currentPif.queryOnce(bT(SET_BREAKPOINT, getPrologFileName(currentIFile), m.getLineNumber(), offset, "Id"));
					} catch (PrologInterfaceException e) {
						Debug.report(e);
					} catch (BadLocationException e) {
						Debug.report(e);
					}
				} else {
					// else, just reset the marker with the same information as before
					try {
						currentPif.queryOnce(bT(SET_BREAKPOINT, getPrologFileName(m.getFile()), m.getLineNumber(), m.getOffset(), "Id"));
					} catch (PrologInterfaceException e) {
						Debug.report(e);
					}
				}
			}

		}
		
		// disable logging of deleted ids
		deletedIds = null;
		markerBackup = null;
		currentIFile = null;
		document = null;
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

	private void addMarker(String fileName, int line, int offset, String id) throws CoreException {
		// called when a breakpoint was added in prolog
		try {
			IFile file = PDTCoreUtils.findFileForLocation(fileName);
			addMarker(file, line, offset, id);
		} catch (IOException e) {
			Debug.report(e);
		}
	}
	
	private void addMarker(IFile file, int line, int offset, String id) throws CoreException {
		HashMap<String, Comparable<?>> attributes = new HashMap<String, Comparable<?>>();
		attributes.put(IMarker.LINE_NUMBER, line);
		attributes.put(IMarker.MESSAGE, "Prolog Breakpoint: line[" + line + "]");
		attributes.put(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
		attributes.put(BREAKPOINT_ID, id);
		attributes.put(BREAKPOINT_OFFSET, offset);
		MarkerUtilities.createMarker(file, attributes, PDT_BREAKPOINT_MARKER);
	}

	private void removeAllBreakpointMarkers() {
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
							// FIXME very slow in the first execution
//							currentPif.queryOnce("current_prolog_flag(version,V)");
							System.out.println(bT(SET_BREAKPOINT, prologFileName, line, offset, "Id"));
//							System.out.println("pif is up? " + currentPif.isUp());
//							AsyncPrologSession async = currentPif.getAsyncSession();
//							async.queryOnce("", bT(SET_BREAKPOINT, prologFileName, line, offset, "_"));
//							currentPif.queryOnce(bT(SET_BREAKPOINT, prologFileName, line, offset, "_"));
							ConsultActionDelegate consultActionDelegate = new ConsultActionDelegate();
							consultActionDelegate.setQuery(bT(SET_BREAKPOINT, prologFileName, line, offset, "_"));
							consultActionDelegate.run();
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

	private void loadBreakpointsFromPif() {
		List<Map<String, Object>> results;
		try {
			results = currentPif.queryAll(bT(BREAKPOINT_PROPERTY, "Id" , "file(File)"),
										  bT(BREAKPOINT_PROPERTY, "Id" , "line_count(Line)"),
										  bT(BREAKPOINT_PROPERTY, "Id" , "character_range(Offset, _)"));
			for (Map<String, Object> result : results) {
				int line = Integer.parseInt(result.get("Line").toString());
				int offset = Integer.parseInt(result.get("Offset").toString());
				String id = result.get("Id").toString();
				try {
					addMarker(result.get("File").toString(), line, offset, id);
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
	
	
	
	@Override
	public void activePrologInterfaceChanged(PrologConsoleEvent e) {
		Object source = e.getSource();
		if (source instanceof PrologConsole){
			if (PDTUtils.checkForActivePif(false)) {
				removePifListener();
				
				PrologConsole console = (PrologConsole) source;
				currentPif = console.getPrologInterface();
				removeAllBreakpointMarkers();
				loadBreakpointsFromPif();
				
				addPifListener();
				
			}
		}
	}

	private void addPifListener() {
		if (currentPif != null) {
			System.out.println("add listener for pif " + currentPif.toString());
			
			currentDispatcher = new PrologEventDispatcher(currentPif,PrologRuntimeUIPlugin.getDefault().getLibraryManager());
			currentPif.addLifeCycleHook(this, BREAKPOINT_LIFECYCLE_HOOK, new String[0]);
			try {
				currentDispatcher.addPrologInterfaceListener("add_breakpoint", this);
				currentDispatcher.addPrologInterfaceListener("remove_breakpoint", this);
				currentDispatcher.addPrologInterfaceListener("file_loaded", this);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
			
		}
	}

	private void removePifListener() {
		if (currentPif != null && currentDispatcher != null) {
			System.out.println("remove listener for pif " + currentPif.toString());
			currentPif.removeLifeCycleHook(BREAKPOINT_LIFECYCLE_HOOK);
			try {
				currentDispatcher.removePrologInterfaceListener("add_breakpoint", this);
				currentDispatcher.removePrologInterfaceListener("remove_breakpoint", this);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
		}
	}

	@Override
	public void consoleRecievedFocus(PrologConsoleEvent e) {
		if (currentPif == null) {
			Object source = e.getSource();
			if (source instanceof PrologConsole){
				PrologConsole console = (PrologConsole) source;
				currentPif = console.getPrologInterface();
				addPifListener();
			}
		}
	}

	@Override
	public void consoleLostFocus(PrologConsoleEvent e) {}

	@Override
	public void consoleVisibilityChanged(PrologConsoleEvent e) {}

	@Override
	public void update(PrologInterfaceEvent e) {
		if (e.getSubject().equals("add_breakpoint")) {
			String id = e.getEvent();
			try {
				Map<String, Object> result = currentPif.queryOnce(bT("pdt_breakpoint_properties", id, "File", "Line", "Offset"));
				String file = result.get("File").toString();
				int line = Integer.parseInt(result.get("Line").toString());
				int offset = Integer.parseInt(result.get("Offset").toString());
				addMarker(file, line, offset, id);
			} catch (PrologInterfaceException e1) {
				Debug.report(e1);
			} catch (CoreException e1) {
				Debug.report(e1);
			}
		} else if (e.getSubject().equals("remove_breakpoint")) {
			String id = e.getEvent();
			removeMarkerWithId(id);
		} else if (e.getSubject().equals("file_loaded")) {
			updateMarkers();
//			System.out.println("update marker for " + e.getEvent());
		}
	}

	private void removeMarkerWithId(String id) {
		try {
			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
			for (IMarker marker : markers) {
				if (marker.getAttribute(BREAKPOINT_ID, "").equals(id)) {
					marker.delete();
					System.out.println("remove marker " + id);
					if (deletedIds != null) {
						deletedIds.add(id);
					}
					return;
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}

	@Override
	public void onInit(PrologInterface pif, PrologSession initSession) throws PrologInterfaceException {}

	@Override
	public void afterInit(PrologInterface pif) throws PrologInterfaceException {}

	@Override
	public void beforeShutdown(PrologInterface pif, PrologSession session) throws PrologInterfaceException {
		System.out.println("beforeShutdown: marker bitte abspeichern");
	}

	@Override
	public void onError(PrologInterface pif) {
		System.out.println("onError: marker bitte abspeichern");
	}

	@Override
	public void setData(Object data) {}

	@Override
	public void lateInit(PrologInterface pif) {}

	
}

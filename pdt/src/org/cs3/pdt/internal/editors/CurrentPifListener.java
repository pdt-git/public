package org.cs3.pdt.internal.editors;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.console.PrologConsole;
import org.cs3.pdt.console.PrologConsoleEvent;
import org.cs3.pdt.console.PrologConsoleListener;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.lifecycle.PrologEventDispatcher;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceEvent;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.PrologInterfaceListener;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;

public class CurrentPifListener implements PrologInterfaceListener, PrologConsoleListener {

	private static final String FILE_LOADED = "file_loaded";
	private static final String PDT_EDIT = "pdt_edit_hook";

	@Override
	public void update(PrologInterfaceEvent e) {
		if (e.getSubject().equals(FILE_LOADED)) {
			fileLoaded(e.getEvent());
		} else if (e.getSubject().equals(PDT_EDIT)) {
			openFileInEditor(e.getEvent());
		}
	}

	private void fileLoaded(String file) {
		file = Util.unquoteStringOrAtom(file);
		String[] parts = file.split("<>");
		for (String s : parts) {
			currentPif.addConsultedFile(s);
		}
		PDTPlugin.getDefault().notifyDecorators();
	}

	public void openFileInEditor(String event) {
		if (event.startsWith("'")) {
			event = event.substring(1);
		}
		if (event.endsWith("'")) {
			event = event.substring(0, event.length() - 1);
		}
		int index = event.lastIndexOf(" ");
		final String fileName = event.substring(0, index);
		final int lineNumber = Integer.parseInt(event.substring(index + 1));
		Debug.debug("Try to open PLEditor for " + fileName + " (line " + lineNumber + ")");
		Runnable r = new Runnable() {
			@Override
			public void run() {
				IEditorPart editorPart = UIUtils.openInEditor(fileName);
				if (editorPart != null && editorPart instanceof PLEditor){
					((PLEditor) editorPart).gotoLine(lineNumber);
				}
			}
		};
		Display.getDefault().syncExec(r);
	}

	PrologInterface currentPif;
	private PrologEventDispatcher currentDispatcher;
	
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
	
	private void addPifListener() {
		if (currentPif != null) {
			Debug.debug("add edit registry listener for pif " + currentPif.toString());
			currentDispatcher = new PrologEventDispatcher(currentPif,PrologRuntimeUIPlugin.getDefault().getLibraryManager());
			try {
				currentDispatcher.addPrologInterfaceListener(PDT_EDIT, this);
				currentDispatcher.addPrologInterfaceListener(FILE_LOADED, this);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
			
		}
	}
	private void removePifListener() {
		if (currentPif != null && currentDispatcher != null) {
			Debug.debug("remove edit registry listener for pif " + currentPif.toString());
			try {
				currentDispatcher.removePrologInterfaceListener(PDT_EDIT, this);
				currentDispatcher.removePrologInterfaceListener(FILE_LOADED, this);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
		}
	}
	@Override
	public void consoleLostFocus(PrologConsoleEvent e) {}

	@Override
	public void consoleVisibilityChanged(PrologConsoleEvent e) {
		PDTPlugin.getDefault().notifyDecorators();
	}

	@Override
	public void activePrologInterfaceChanged(PrologConsoleEvent e) {
		Object source = e.getSource();
		if (source instanceof PrologConsole){
			
			PDTPlugin.getDefault().notifyDecorators();

//			if (PDTUtils.checkForActivePif(false)) {
				removePifListener();
				
				PrologConsole console = (PrologConsole) source;
				currentPif = console.getPrologInterface();
				
				addPifListener();
				
				updateEntryPoints();
//			}
		}
	}

	private void updateEntryPoints() {
		try {
			currentPif.queryOnce(bT("remove_entry_points", "_"));
			
			for (IFile file : PrologConsolePlugin.getDefault().getEntryPoints()) {
				try {
					String prologFileName = Util.prologFileName(file.getLocation().toFile().getCanonicalFile());
					currentPif.queryOnce(bT("add_entry_point", Util.quoteAtom(prologFileName)));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
		
	}

}

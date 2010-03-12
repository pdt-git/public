package org.cs3.pdt.runtime.socket.preferences;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class PreferencePageSocket
	extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

	public PreferencePageSocket() {
		super(GRID);
		setPreferenceStore(PrologRuntimePlugin.getDefault().getPreferenceStore());
		setDescription("Preferences for the Socket-Runtime");
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		
	
		// eg. kill or /usr/bin/kill on most systems",
//		addField(new FileFieldEditor(ProcessKiller.PREF_KILLCOMMAND, "command to kill processes", getFieldEditorParent()));

		
		// If true, the PIF will try to pool and reuse disposed sessions to
		// reduce connection overhead.
		addField(new BooleanFieldEditor(SocketPrologInterfacePreferences.PREF_USE_POOL, "Use session pooling", getFieldEditorParent()));

	
		// When enabled, the server process will produce rather verbose log
		// files below the systems temp directory.
		addField(new BooleanFieldEditor(SocketPrologInterfacePreferences.PREF_CREATE_SERVER_LOGS, "Create server debug logs", getFieldEditorParent()));

		// The port the PIF server is listening on
		IntegerFieldEditor port = new IntegerFieldEditor(SocketPrologInterfacePreferences.PREF_PORT, "Server port", getFieldEditorParent());
		port.setEnabled(false, getFieldEditorParent());
		addField(port);
		
		// Usefull for windows users who are tired of plwin windows cluttering
		// their system tray.
		// Note: this only works with the plwin executable.
		addField(new BooleanFieldEditor(SocketPrologInterfacePreferences.PREF_HIDE_PLWIN, "Hide plwin (windows only)", getFieldEditorParent()));
		
		DirectoryFieldEditor ffe = new DirectoryFieldEditor(SocketPrologInterfacePreferences.PREF_SERVER_LOGDIR, "Server-Log file location", getFieldEditorParent());
		//ffe.setPropertyChangeListener(debugPropertyChangeListener);
		//ffe.getPreferenceStore().addPropertyChangeListener(debugPropertyChangeListener);
		addField(ffe);	

	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
}
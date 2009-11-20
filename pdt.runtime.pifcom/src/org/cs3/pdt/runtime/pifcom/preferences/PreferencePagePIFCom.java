package org.cs3.pdt.runtime.pifcom.preferences;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
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

public class PreferencePagePIFCom
	extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

	public PreferencePagePIFCom() {
		super(GRID);
		setPreferenceStore(PrologRuntimePlugin.getDefault().getPreferenceStore());
		setDescription("Preferences for the PIFCOM-Runtime");
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {

		// The port the PIF server is listening on
		IntegerFieldEditor tcpport = new IntegerFieldEditor(PIFComConstants.PIF_PORT_TCP , "Server port TCP", getFieldEditorParent());
		//tcpport.setEnabled(false, getFieldEditorParent());
		addField(tcpport);
	
		// The port the PIF server is listening on
		IntegerFieldEditor udpport = new IntegerFieldEditor(PIFComConstants.PIF_PORT_UDP , "Server port UDP", getFieldEditorParent());
		//udpport.setEnabled(false, getFieldEditorParent());
		addField(udpport);
	
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
}
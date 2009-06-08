package org.cs3.pdt.console.preferences;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FontFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class PreferencePageFontColor extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	private ColorFieldEditor cfe_error;
	private ColorFieldEditor cfe_warn;
	private ColorFieldEditor cfe_info;
	private ColorFieldEditor cfe_dbg;
	private BooleanFieldEditor bfe_inter_start;
	private BooleanFieldEditorWithAccessToCheckBox bfe_showColors;	
	
	public PreferencePageFontColor() {
		super(GRID);
		setPreferenceStore(PrologConsolePlugin.getDefault().getPreferenceStore());
		setDescription("Console font and color preferences");
	}


	private void initColorFieldEditors(Boolean show_colors){

		cfe_error.setEnabled(show_colors, getFieldEditorParent());
		cfe_warn.setEnabled(show_colors, getFieldEditorParent());
		cfe_info.setEnabled(show_colors, getFieldEditorParent());
		cfe_dbg.setEnabled(show_colors, getFieldEditorParent());
		bfe_inter_start.setEnabled(show_colors, getFieldEditorParent());
		
	}
	
	
	class BooleanFieldEditorWithAccessToCheckBox extends BooleanFieldEditor{
		
		
		public BooleanFieldEditorWithAccessToCheckBox() {
			super();
		}

		public BooleanFieldEditorWithAccessToCheckBox(String name, String label, Composite parent) {
			super(name, label, parent);
		}

		public BooleanFieldEditorWithAccessToCheckBox(String name, String labelText, int style, Composite parent) {
			super(name, labelText, style, parent);
		}

		public Button getCheckBox(Composite parent){
			return getChangeControl(parent);
		}
	}
	
	
	
	
	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	public void createFieldEditors() {
		
		
		addField(new FontFieldEditor(PreferenceConstants.PREF_CONSOLE_FONT, "Console font:", getFieldEditorParent()));

		bfe_showColors = new BooleanFieldEditorWithAccessToCheckBox(PreferenceConstants.PREF_CONSOLE_SHOW_COLORS, "Show colors", getFieldEditorParent());
		cfe_error = new ColorFieldEditor(PreferenceConstants.PREF_CONSOLE_COLOR_ERROR, "Error color", getFieldEditorParent());
		cfe_warn = new ColorFieldEditor(PreferenceConstants.PREF_CONSOLE_COLOR_WARNING, "Warning color", getFieldEditorParent());
		cfe_info = new ColorFieldEditor(PreferenceConstants.PREF_CONSOLE_COLOR_INFO, "Information color", getFieldEditorParent());
		cfe_dbg = new ColorFieldEditor(PreferenceConstants.PREF_CONSOLE_COLOR_DEBUG, "Debug color", getFieldEditorParent());
		bfe_inter_start = new BooleanFieldEditor(PreferenceConstants.PREF_CONSOLE_COLORS_THREESTARS, "Interprete *** as 'Information' ", getFieldEditorParent());
	
		addField(bfe_showColors);
		final Button checkBox = bfe_showColors.getCheckBox(getFieldEditorParent());
		 
		
         
		 
		 checkBox.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent e) {

                    boolean isSelected = checkBox.getSelection();
                    initColorFieldEditors(isSelected);
                }
            });
		

		
		addField(cfe_error);
		addField(cfe_warn);
		addField(cfe_info);
		addField(cfe_dbg);
		addField(bfe_inter_start);

		// Werte am Anfang initialisieren
		IPreferenceStore store = PrologConsolePlugin.getDefault().getPreferenceStore();
		Boolean show_colors = store.getBoolean(PreferenceConstants.PREF_CONSOLE_SHOW_COLORS);		
		initColorFieldEditors(show_colors);
		

		
		
		
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}

}
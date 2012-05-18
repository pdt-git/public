package org.cs3.pdt.console.preferences;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.ui.preferences.MyBooleanFieldEditor;
import org.cs3.pdt.ui.preferences.MyColorFieldEditor;
import org.cs3.pdt.ui.preferences.MyFontFieldEditor;
import org.cs3.pdt.ui.preferences.StructuredFieldEditorPreferencePage;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Group;
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

public class PreferencePageFontColor extends StructuredFieldEditorPreferencePage implements IWorkbenchPreferencePage {

	private ColorFieldEditor cfe_error;
	private ColorFieldEditor cfe_warn;
	private ColorFieldEditor cfe_info;
	private ColorFieldEditor cfe_dbg;
	private BooleanFieldEditor bfe_inter_start;
	
	public PreferencePageFontColor() {
		super(GRID);
		setPreferenceStore(PrologConsolePlugin.getDefault().getPreferenceStore());
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
		
		
		Group fontGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		fontGroup.setText("Console font");
		addField(new MyFontFieldEditor(PDTConsole.PREF_CONSOLE_FONT, "Console font:", fontGroup));
		
		Group colourGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		colourGroup.setText("Colour output line starting with ...");
		
		cfe_error = new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_ERROR, "ERROR", colourGroup);
		cfe_warn = new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_WARNING, "WARNING", colourGroup);
		cfe_info = new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_INFO, "INFO", colourGroup);
		cfe_dbg = new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_DEBUG, "DEBUG", colourGroup);
		bfe_inter_start = new MyBooleanFieldEditor(PDTConsole.PREF_CONSOLE_COLORS_THREESTARS, "Treat '***' as 'INFO'", colourGroup);
		
		addField(cfe_error);
		addField(cfe_warn);
		addField(cfe_info);
		addField(cfe_dbg);
		addField(bfe_inter_start);
	}

    /*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(IWorkbench workbench) {
	}

}
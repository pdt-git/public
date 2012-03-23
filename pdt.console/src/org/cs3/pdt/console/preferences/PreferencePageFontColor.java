package org.cs3.pdt.console.preferences;

import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FontFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
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

public class PreferencePageFontColor extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	private ColorFieldEditor cfe_error;
	private ColorFieldEditor cfe_warn;
	private ColorFieldEditor cfe_info;
	private ColorFieldEditor cfe_dbg;
	private BooleanFieldEditor bfe_inter_start;
	private BooleanFieldEditorWithAccessToCheckBox bfe_showColors;
	private Group colourGroup;	
	
	public PreferencePageFontColor() {
		super(GRID);
		setPreferenceStore(PrologConsolePlugin.getDefault().getPreferenceStore());
		setDescription("Appearance preferences");
	}


	private void initColorFieldEditors(Boolean show_colors){

		cfe_error.setEnabled(show_colors, getColourGroup());
		cfe_warn.setEnabled(show_colors, getColourGroup());
		cfe_info.setEnabled(show_colors, getColourGroup());
		cfe_dbg.setEnabled(show_colors, getColourGroup());
		bfe_inter_start.setEnabled(show_colors, getColourGroup());
		
	}
	
	
	private Composite getColourGroup() {
		return colourGroup;
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
	@Override
	public void createFieldEditors() {
		
		
		Group fontGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		fontGroup.setText("Console font");
		fontGroup.setLayout(new GridLayout());
		fontGroup.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		addField(new FontFieldEditor(PDTConsole.PREF_CONSOLE_FONT, "Console font:", fontGroup) {
			@Override
			protected void adjustForNumColumns(int numColumns) {}
		});
		groups.add(fontGroup);
		
		bfe_showColors = new BooleanFieldEditorWithAccessToCheckBox(PDTConsole.PREF_CONSOLE_SHOW_COLORS, "Show colors", getFieldEditorParent());

		colourGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		colourGroup.setText("Colour output line starting with ...");
		colourGroup.setLayout(new GridLayout(2, false));
		colourGroup.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		groups.add(colourGroup);
		
		cfe_error = new ColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_ERROR, "ERROR", colourGroup);
		cfe_warn = new ColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_WARNING, "WARNING", colourGroup);
		cfe_info = new ColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_INFO, "INFORMATION", colourGroup);
		cfe_dbg = new ColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_DEBUG, "DEBUG", colourGroup);
		bfe_inter_start = new BooleanFieldEditor(PDTConsole.PREF_CONSOLE_COLORS_THREESTARS, "Treat '***' as 'INFO'", colourGroup);
		colourGroup.setLayout(new GridLayout(2, false));
		colourGroup.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		addField(bfe_showColors);
		final Button checkBox = bfe_showColors.getCheckBox(getFieldEditorParent());
		
		checkBox.addSelectionListener(new SelectionAdapter() {
			@Override
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
		Boolean show_colors = store.getBoolean(PDTConsole.PREF_CONSOLE_SHOW_COLORS);		
		initColorFieldEditors(show_colors);
	}

    private List<Group> groups = new ArrayList<Group>();
	
	@Override
    protected void adjustGridLayout() {
		super.adjustGridLayout();
		int numColumns = ((GridLayout) getFieldEditorParent().getLayout()).numColumns;
		for (Group group: groups) {
			((GridLayout)group.getLayout()).numColumns = numColumns;
		}
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
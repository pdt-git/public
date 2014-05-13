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

package org.cs3.pdt.console.internal.preferences;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.preferences.MyBooleanFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyColorFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyFileFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyFontFieldEditor;
import org.cs3.prolog.ui.util.preferences.StructuredFieldEditorPreferencePage;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.swt.SWT;
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

public class PreferencePageMain extends StructuredFieldEditorPreferencePage implements IWorkbenchPreferencePage {

	private ColorFieldEditor cfe_error;
	private ColorFieldEditor cfe_warn;
	private ColorFieldEditor cfe_info;
	private ColorFieldEditor cfe_dbg;
	private BooleanFieldEditor bfe_inter_start;
	
	public PreferencePageMain() {
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
		
		
		//The Prolog Console uses this to save its command history.\n
		//Just leave it empty if you do not want the command history to be persistent.
		addField(new FileFieldEditorWithEnsureFileExists(PDTConsole.PREF_CONSOLE_HISTORY_FILE,"History File",getFieldEditorParent()));
		
		adjustLayoutForElement(fontGroup);
		adjustLayoutForElement(colourGroup);
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
	
	
	
	class FileFieldEditorWithEnsureFileExists extends MyFileFieldEditor {

		public FileFieldEditorWithEnsureFileExists(String name, String labelText, Composite parent) {
			super(name, labelText, parent);
		}
		
		@Override
		protected boolean checkState() {
			return ensureFileExists();// && super.checkState();
		}
		
		private boolean ensureFileExists() {
			
			String msg = null;
			String value = getTextControl().getText();
			
			if (value == null) {
				msg = "History File must not be null";
			}
			if (value.length() == 0) {
				msg = "History File must not be empty";
			}
			File f = new File(value);
			if (!f.isAbsolute()) {
				msg = "History File must be an absolute path";
			}
			if (f.isDirectory()) {
				msg = "History File exists, but is a directory";
			}
			if (!f.exists()) {
				try {
					if (!f.createNewFile()) {

					}
				} catch (IOException e) {
					Debug.report(e);
					msg = "could not create History File";
				}

			}
			if (!f.canWrite()) {
				msg = "History File exists, but is not writable";
			}
			
			
			if (msg != null) { // error
		        showErrorMessage(msg);
		        return false;
			}

	        // OK!
	        clearErrorMessage();
	        return true;
		}

	}
	
	


}



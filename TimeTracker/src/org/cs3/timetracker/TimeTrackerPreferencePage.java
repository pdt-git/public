/*
 * Created on 28.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.timetracker;


import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * @author trho
 */
public class TimeTrackerPreferencePage 
    extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

//		public static final String P_WORKING_DIR = "workingDir";
//  public static final String P_USER = "userRubPreference";
		public static final String P_IS_COUNTING_UP = "iscountingup";
		
		public TimeTrackerPreferencePage() {
			super(GRID);
			setPreferenceStore(TimeTrackerPlugin.getDefault().getPreferenceStore());
			setDescription("Preferences for the Time Tracker Plugin");
			initializeDefaults();
		}
		/**
		 * Sets the default values of the preferences.
		 */
		private void initializeDefaults() {
			IPreferenceStore store = getPreferenceStore();
			store.setDefault(P_IS_COUNTING_UP, false);
//      store.setDefault(P_USER, "This field is useless");
//      store.setDefault(P_RESTORE, true);  
		}

		/**
		 * Creates the field editors. Field editors are abstractions of
		 * the common GUI blocks needed to manipulate various types
		 * of preferences. Each field editor knows how to save and
		 * restore itself.
		 */
		public void createFieldEditors() {
			addField(
					new BooleanFieldEditor(
							P_IS_COUNTING_UP,
							"Count the time up (If you deactivate this, it will count down from 3 Minutes to zero.)",
							getFieldEditorParent()));
			
//      addField(
//          new ButtonFieldEditor(
//              P_USER,
//              "Reset user.rub file.",     
//              getFieldEditorParent()) { }
//      );
			
//      addField(
//          new BooleanFieldEditor(
//              P_RESTORE,
//              "Restore trees from previous session",
//              getFieldEditorParent())
//      );
			
			

		}

		public void init(IWorkbench workbench) {
		    setPreferenceStore(TimeTrackerPlugin.getDefault().getPreferenceStore());
		}
		/**
		 * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
		 */
		protected void performDefaults() {
			TimeTrackerPlugin.getDefault().updatePreferences();  
			super.performDefaults();
		}

		/**
		 * @see org.eclipse.jface.preference.IPreferencePage#performOk()
		 */
		public boolean performOk() {
			
			boolean res = super.performOk();
			TimeTrackerPlugin.getDefault().updatePreferences();
			return res;
		}

		
}

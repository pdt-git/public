/*
 * Created on 28.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl;


import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;


/**
 * @author trho
 *
 */
public class PDTPreferencePage 
    extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

//		public static final String P_WORKING_DIR = "workingDir";
//  public static final String P_USER = "userRubPreference";
		public static final String P_JAVA_LANG_CACHING = "javalangfactcaching";
		public static final String P_COMPLETE_CACHING = "completefactcaching";
		public static final String P_SINGLETON_DTM = "singletoncheckfordtm"; 
		public static final String P_PROLOG_SERVER_PORT = "prologserverport";
		public static final String P_PROLOG_DEBUG_LEVEL = "prologdebuglevel"; 
		
		public PDTPreferencePage() {
			super(GRID);
			setPreferenceStore(PDTPlugin.getDefault().getPreferenceStore());
			setDescription("Preferences for the PDTPlugin Plugin");
			//initializeDefaults();
		}
		/**
		 * Sets the default values of the preferences.
		 */
//		private void initializeDefaults() {
//			IPreferenceStore store = getPreferenceStore();
//			store.setDefault(P_COMPLETE_CACHING, false);
//			store.setDefault(P_JAVA_LANG_CACHING, false);
//			store.setDefault(P_SINGLETON_DTM, false);
//
//
////      store.setDefault(P_USER, "This field is useless");
////      store.setDefault(P_RESTORE, true);  
//		}

		/**
		 * Creates the field editors. Field editors are abstractions of
		 * the common GUI blocks needed to manipulate various types
		 * of preferences. Each field editor knows how to save and
		 * restore itself.
		 */
		public void createFieldEditors() {
			addField(
					new BooleanFieldEditor(
							P_COMPLETE_CACHING,
							"activate complete fact caching",
							getFieldEditorParent()));
			addField(
					new BooleanFieldEditor(
							P_JAVA_LANG_CACHING,
							"activate fact caching for the package java lang",
							getFieldEditorParent()));

			addField(
					new BooleanFieldEditor(
							P_SINGLETON_DTM,
							"singleton check for don't-tell-me variables (start with '_')",
							getFieldEditorParent()));
			addField(
					new IntegerFieldEditor(
							P_PROLOG_SERVER_PORT,
							"prolog server port",
							getFieldEditorParent()));
			addField(new RadioGroupFieldEditor(
							P_PROLOG_DEBUG_LEVEL, 
							"debug level",
							4,
							new String[][] {
									{ "error", "1" }, 
									{ "warning", "2" }, 
									{ "info", "3" }, 
									{ "debug", "4" } },
				getFieldEditorParent(),true));

			
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
		    setPreferenceStore(PDTPlugin.getDefault().getPreferenceStore());
		}
		/**
		 * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
		 */
		protected void performDefaults() {
			PDTPlugin.getDefault().updatePreferences();  
			super.performDefaults();
		}

		/**
		 * @see org.eclipse.jface.preference.IPreferencePage#performOk()
		 */
		public boolean performOk() {
			
			boolean res = super.performOk();
			PDTPlugin.getDefault().updatePreferences();
			return res;
		}

		
}

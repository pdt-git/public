/*
 * Created on 28.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal.preferences;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.prolog.Option;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * @author trho
 *  
 */
public class PreferencePage extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    //		public static final String P_WORKING_DIR = "workingDir";
    //  public static final String P_USER = "userRubPreference";

    public PreferencePage() {
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
    //// store.setDefault(P_USER, "This field is useless");
    //// store.setDefault(P_RESTORE, true);
    //		}
    /**
     * Creates the field editors. Field editors are abstractions of the common
     * GUI blocks needed to manipulate various types of preferences. Each field
     * editor knows how to save and restore itself.
     */
    public void createFieldEditors() {
        
        Composite parent = getFieldEditorParent();
        addField(new StringFieldEditor(PDT.PREF_PIF_IMPLEMENTATION,"PrologInterfaceFactory implementation",parent));
        IPreferencesService service = Platform.getPreferencesService();
        String qualifier = PDTPlugin.getDefault().getBundle().getSymbolicName();
        String impl= service.getString(qualifier,PDT.PREF_PIF_IMPLEMENTATION,null,  null);         
        Option[] options = PrologInterfaceFactory.newInstance(impl).getOptions();
        for(int i=0;i<options.length;i++){
            String name = options[i].getId();
            String label = options[i].getLabel();
            switch(options[i].getType()){
            	case Option.DIR:
            	    addField(new DirectoryFieldEditor(name,label,parent));
            	break;
            	case Option.FLAG:
            	    addField(new BooleanFieldEditor(name,label,parent));
            	break;
            	case Option.NUMBER:
            	    addField(new IntegerFieldEditor(name,label,parent));
            	default:
            	    addField(new StringFieldEditor(name,label,parent));
            		break;
            		
            }
        }
        addField(new IntegerFieldEditor(PDT.PREF_CONSOLE_PORT,
                "Port for console IO", parent));

        FileListEditor consultPathEditor = new FileListEditor(
                PDT.PREF_CONSULT_PATH,
                "List of files/directories to auto-consult", "add pl-file",
                parent);        
        consultPathEditor.setFilterExtensions(new String[]{".pl"});
        addField(consultPathEditor);

        addField(new RadioGroupFieldEditor(PDT.PREF_DEBUG_LEVEL,
                "debug level", 4, new String[][] { { "error", "ERROR" },
                        { "warning", "WARNING" }, { "info", "INFO" },
                        { "debug", "DEBUG" } }, parent, true));

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
        //PDTPlugin.getDefault().updatePreferences();
        super.performDefaults();
    }

    /**
     * @see org.eclipse.jface.preference.IPreferencePage#performOk()
     */
    public boolean performOk() {

        boolean res = super.performOk();
        PDTPlugin.getDefault().reconfigure();
        return res;
    }

}
/*
 * Created on 28.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.runtime.internal.preferences;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Option;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
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
        setPreferenceStore(PrologRuntimePlugin.getDefault().getPreferenceStore());
        setDescription("Preferences for the PDTPlugin Plugin");
    }

  
    /**
     * Creates the field editors. Field editors are abstractions of the common
     * GUI blocks needed to manipulate various types of preferences. Each field
     * editor knows how to save and restore itself.
     */
    public void createFieldEditors() {
        
        Composite parent = getFieldEditorParent();
        IPreferencesService service = Platform.getPreferencesService();
        PrologRuntimePlugin plugin = PrologRuntimePlugin.getDefault();
        String qualifier = plugin.getBundle().getSymbolicName();
        String impl= service.getString(qualifier,PrologRuntime.PREF_PIF_IMPLEMENTATION,null,  null);         
        
        Option[] options = plugin.getOptions();
        addEditorsForOptions(parent, options);
        options = PrologInterfaceFactory.newInstance(impl).getOptions();
        addEditorsForOptions(parent, options);
        
    }

    private void addEditorsForOptions(Composite parent, Option[] options) {
        FieldEditor editor = null;
        for(int i=0;i<options.length;i++){
            String name = options[i].getId();
            String label = options[i].getLabel();
            
			//FIXME: directory does not exist at plugin startup time
			//Workaround:
			if(label.equals("Metadata Store Dir"))
				editor = new StringFieldEditor(name,label,parent);
			else
            switch(options[i].getType()){
            	case Option.DIR:
            	    editor = new DirectoryFieldEditor(name,label,parent);            	    
            	break;
            	case Option.FLAG:
            	    editor = new BooleanFieldEditor(name,label,parent);
            	break;
            	case Option.NUMBER:
            	    editor = new IntegerFieldEditor(name,label,parent);
            	break;
            	case Option.ENUM:
            	    editor = new RadioGroupFieldEditor(name,label,4,options[i].getEnumValues(),parent,true);
            	break;
            	default:
            	    editor = new StringFieldEditor(name,label,parent);
            		break;
            		
            }
            //disable the editor, if the value is overridden per sys prop.
            editor.setEnabled(System.getProperty(name)==null,parent);
            addField(editor);
        }
    }

    public void init(IWorkbench workbench) {
        setPreferenceStore(PrologRuntimePlugin.getDefault().getPreferenceStore());
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
        PrologRuntimePlugin.getDefault().reconfigure();
        return res;
    }

}
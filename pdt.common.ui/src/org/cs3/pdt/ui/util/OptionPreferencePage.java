package org.cs3.pdt.ui.util;


import org.cs3.pl.common.Option;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public abstract class OptionPreferencePage extends FieldEditorPreferencePage implements
IWorkbenchPreferencePage{

	private Option[] options;

	/** note: if you use this constructor, it is up to you to configure the PreferencePage accordingly.*/
	public OptionPreferencePage(){
		super(GRID);
	}
	
	public OptionPreferencePage(IPreferenceStore store, Option[] options, String descr){
		super(GRID);
        this.setOptions(options);
		setPreferenceStore(store);
        setDescription(descr);
	}
	
	
	
	/**
     * Creates the field editors. Field editors are abstractions of the common
     * GUI blocks needed to manipulate various types of preferences. Each field
     * editor knows how to save and restore itself.
     */
    public void createFieldEditors() {        
        Composite parent = getFieldEditorParent();
        addEditorsForOptions(parent, getOptions());
    }

    private void addEditorsForOptions(Composite parent, final Option[] options) {
        FieldEditor editor = null;
        for(int i=0;i<options.length;i++){
            
        	if(!options[i].isVisible()){
            	continue;
            }
        	
        	String name = options[i].getId();
            String label = options[i].getLabel();
            
            final int j =i;
            switch(options[i].getType()){
            	case Option.DIR:
            	    editor = new DirectoryFieldEditor(name,label,parent){
            		protected boolean doCheckState() {
            			String errmsg=options[j].validate(getStringValue());
            			if(errmsg==null){
            				return super.doCheckState();
            			}
            			if(errmsg.length()!=0){
            				setErrorMessage(errmsg);
            				return false;
            			}
            			return true;
            		}
            	};            	    
            	break;
            	case Option.FLAG:
            	    editor = new BooleanFieldEditor(name,label,parent);
            	    //TODO: flags are not validated right now. 
            	break;
            	case Option.NUMBER:
            	    editor = new IntegerFieldEditor(name,label,parent){
                		protected boolean doCheckState() {
                			String errmsg=options[j].validate(getStringValue());
                			if(errmsg==null){
                				return super.doCheckState();
                			}
                			if(errmsg.length()!=0){
                				setErrorMessage(errmsg);
                				return false;
                			}
                			return true;
                		}
                	};
            	break;
            	case Option.ENUM:
            	    editor = new RadioGroupFieldEditor(name,label,4,options[i].getEnumValues(),parent,true);
            	    //TODO: enums are not validated right now. 
            	break;
            	default:
            	    editor = new StringFieldEditor(name,label,parent){
                		protected boolean doCheckState() {
                			String errmsg=options[j].validate(getStringValue());
                			if(errmsg==null){
                				return super.doCheckState();
                			}
                			if(errmsg.length()!=0){
                				setErrorMessage(errmsg);
                				return false;
                			}
                			return true;
                		}
                	};
            		break;
            		
            }
            //disable the editor, if the value is overridden per sys prop.
            editor.setEnabled(System.getProperty(name)==null,parent);
            addField(editor);
        }
    }
    public void init(IWorkbench workbench) {
        //setPreferenceStore(PDTPlugin.getDefault().getPreferenceStore());
    }

    /**
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    protected void performDefaults() {
        super.performDefaults();
    }

    /**
     * @see org.eclipse.jface.preference.IPreferencePage#performOk()
     */
    public boolean performOk() {

        boolean res = super.performOk();
        reconfigure();
        return res;
    }
	protected abstract void reconfigure();
	protected void setOptions(Option[] options) {
		this.options = options;
	}
	protected Option[] getOptions() {
		return options;
	} 


}

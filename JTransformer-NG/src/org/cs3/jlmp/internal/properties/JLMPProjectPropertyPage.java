/*
 */
package org.cs3.jlmp.internal.properties;

import java.util.HashMap;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPProject;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PropertyPage;

/**
 */
public class JLMPProjectPropertyPage extends PropertyPage implements
        IWorkbenchPropertyPage {

    private HashMap editors = new HashMap();

    /**
     *  
     */
    public JLMPProjectPropertyPage() {
        super();
        // TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
     */
    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        composite.setLayout(layout);
        GridData data = new GridData(GridData.FILL);
        data.grabExcessHorizontalSpace = true;
        data.verticalAlignment = GridData.FILL;
        data.horizontalAlignment = GridData.FILL;
        composite.setLayoutData(data);
        
        JLMPProject jlmpProject = getJLMPProject();
        Option[] options = jlmpProject.getOptions();
        addEditorsForOptions(composite, options);
        load();
        return composite;
    }

    private JLMPProject getJLMPProject() {
        JLMPProject jlmpProject;
        IProject project = (IProject) getElement();
        try {
            return (JLMPProject) project.getNature(JLMP.NATURE_ID);
        } catch (CoreException e) {
            Debug.report(e);
            return null;
        }
    }

    private void addEditorsForOptions(Composite parent, Option[] options) {
        PropertyEditor editor = null;
        for (int i = 0; i < options.length; i++) {            
            editor=OptionEditor.create(parent, options[i]);
            //disable the editor, if the value is overridden per sys prop.
            editor.setEnabled(System.getProperty(editor.getKey()) == null);
            editors.put(editor.getKey(), editor);
            editor.addPropertyChangeListener(new IPropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent e) {
                    validate((PropertyEditor)e.getSource());
                }
                
            });
            editor.getControl().setToolTipText(options[i].getDescription());
        }
    }
    private void validate(PropertyEditor editor) {
        String error = editor.validate();
        setErrorMessage(error);
        setValid(error==null);
        updateApplyButton();
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.IPreferencePage#performOk()
     */
    public boolean performOk() {
        Option[] options = getJLMPProject().getOptions();
        for (int i = 0; i < options.length; i++) {
            Option option = options[i];
            String id = option.getId();
            PropertyEditor editor = (PropertyEditor) editors.get(id);
            String value = editor.getValue();
            IProject project = getJLMPProject().getProject();
            try {
                project.setPersistentProperty(new QualifiedName("", id), value);
            } catch (CoreException e) {
                Debug.report(e);
                setErrorMessage("ERROR: could not set property " + id
                        + "\nSee log for details.");
                return false;
            }
            getJLMPProject().reconfigure();
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    protected void performDefaults() {
        super.performDefaults();
        Option[] options = getJLMPProject().getOptions();
        for (int i = 0; i < options.length; i++) {
            Option option = options[i];
            String id = option.getId();
            PropertyEditor editor = (PropertyEditor) editors.get(id);
            editor.revertToDefault();
        }
    }

    private void load() {
        Option[] options = getJLMPProject().getOptions();
        for (int i = 0; i < options.length; i++) {
            Option option = options[i];
            String id = option.getId();
            PropertyEditor editor = (PropertyEditor) editors.get(id);

            
            try {
                String value = getJLMPProject().getPreferenceValue(id,"");
                editor.setValue(value);
            } catch (CoreException e) {
                Debug.report(e);
                setErrorMessage("ERROR: could not read property " + id
                        + "\nSee log for details.");
                
            }
        }

    }

    
}

/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 */
package org.cs3.prolog.ui.util;

import java.util.HashMap;

import org.cs3.prolog.common.Option;
import org.cs3.prolog.common.OptionProvider;
import org.cs3.prolog.common.logging.Debug;
import org.eclipse.core.runtime.IAdaptable;
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
public abstract class OptionProviderPropertyPage extends PropertyPage implements
        IWorkbenchPropertyPage {

    private HashMap<String, PropertyEditor> editors = new HashMap<String, PropertyEditor>();
	

    /**
     *  
     */
    public OptionProviderPropertyPage() {
        super();
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
     */
    @Override
	protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        composite.setLayout(layout);
        GridData data = new GridData(GridData.FILL);
        data.grabExcessHorizontalSpace = true;
        data.verticalAlignment = GridData.FILL;
        data.horizontalAlignment = GridData.FILL;
        composite.setLayoutData(data);
        
        OptionProvider jtransformerProject = getOptionProvider();
        Option[] options = jtransformerProject.getOptions();
        addEditorsForOptions(composite, options);
        load();
        return composite;
    }

    private OptionProvider getOptionProvider(){
    	return getOptionProvider(getElement());
    }
    protected abstract OptionProvider getOptionProvider(IAdaptable element);

   

	private void addEditorsForOptions(Composite parent, Option[] options) {
        PropertyEditor editor = null;
        for (int i = 0; i < options.length; i++) {            
            editor=OptionEditor.create(parent, options[i]);
            //disable the editor, if the value is overridden per sys prop.
            editor.setEnabled(System.getProperty(editor.getKey()) == null);
            editors.put(editor.getKey(), editor);
            editor.addPropertyChangeListener(new IPropertyChangeListener() {
                @Override
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
    @Override
	public boolean performOk() {
        Option[] options = getOptionProvider().getOptions();
        
        for (int i = 0; i < options.length; i++) {
            Option option = options[i];
            String id = option.getId();
            PropertyEditor editor = editors.get(id);
            String value = editor.getValue();
            
            try {
            	getOptionProvider().setPreferenceValue(id, value);
            } catch (Throwable e) {
                Debug.report(e);
                setErrorMessage("ERROR: could not set property " + id
                        + "\nSee log for details.");
                return false;
            }
            getOptionProvider().reconfigure();
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
	protected void performDefaults() {
        super.performDefaults();
        Option[] options = getOptionProvider().getOptions();
        for (int i = 0; i < options.length; i++) {
            Option option = options[i];
            String id = option.getId();
            PropertyEditor editor = editors.get(id);
            editor.revertToDefault();
        }
    }

    private void load() {
        Option[] options = getOptionProvider().getOptions();
        for (int i = 0; i < options.length; i++) {
            Option option = options[i];
            String id = option.getId();
            PropertyEditor editor = editors.get(id);

            
            try {
                String value = getOptionProvider().getPreferenceValue(id,"");
                editor.setValue(value);
            } catch (Throwable e) {
                Debug.report(e);
                setErrorMessage("ERROR: could not read property " + id
                        + "\nSee log for details.");
                
            }
        }

    }

    
}



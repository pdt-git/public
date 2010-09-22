/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pdt.ui.util;

import java.util.HashMap;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.OptionProvider;
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
        // TODO Auto-generated constructor stub
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

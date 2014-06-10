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
package org.cs3.pdt.connector.util;

import org.cs3.prolog.common.Option;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

/**
 */
public class FlagEditor extends OptionEditor implements PropertyEditor {
    private static final int TEXT_FIELD_WIDTH = 20;
    private Button valueCheckButton;
    public FlagEditor(Composite parent, Option option) {
        super(parent, option);

    }

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.OptionEditor#createControls(org.eclipse.swt.widgets.Composite)
     */
    @Override
	protected void createControls(Composite composite) {
        GridLayout layout = new GridLayout();
        layout.numColumns = 1;
        composite.setLayout(layout);
        
        GridData data = new GridData(GridData.FILL);
        data.grabExcessHorizontalSpace = true;
        data.verticalAlignment = GridData.FILL;
        data.horizontalAlignment = GridData.FILL;
        composite.setLayoutData(data);


        valueCheckButton = new Button(composite, SWT.CHECK);
        valueCheckButton.setText(option.getLabel());
        GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        gd.widthHint = convertWidthInCharsToPixels(TEXT_FIELD_WIDTH);
        valueCheckButton.setLayoutData(gd);
        valueCheckButton.addSelectionListener(new SelectionListener() {
            String old="";
            @Override
			public void widgetSelected(SelectionEvent e) {
                String newValue = valueCheckButton.getSelection() ? "true":"false";
                if(old.equals(newValue)) {
                    return;
                }
                firePropertyChange(old,newValue);
                old=newValue;
            }

            @Override
			public void widgetDefaultSelected(SelectionEvent e) {             
                widgetSelected( e);
            }
        });
    }

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#setValue(java.lang.String)
     */
    @Override
	public void setValue(String value) {
        valueCheckButton.setSelection(Boolean.valueOf(value).booleanValue());
    }

    
    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#getValue()
     */
    @Override
	public String getValue() {      
        return valueCheckButton.getSelection() ? "true":"false";
    }

}



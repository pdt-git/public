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
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 */
public class StringEditor extends OptionEditor implements PropertyEditor {
    private static final int TEXT_FIELD_WIDTH = 20;
    private Text valueField;
    public StringEditor(Composite parent, Option option) {
        super(parent, option);

    }

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.OptionEditor#createControls(org.eclipse.swt.widgets.Composite)
     */
    @Override
	protected void createControls(Composite composite) {
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        composite.setLayout(layout);
        
        GridData data = new GridData(GridData.FILL);
        data.grabExcessHorizontalSpace = true;
        data.verticalAlignment = GridData.FILL;
        data.horizontalAlignment = GridData.FILL;
        composite.setLayoutData(data);

        //info text
//        Label info = new Label(composite, SWT.READ_ONLY | SWT.MULTI | SWT.WRAP);        
//        info.setText(option.getDescription());
//        GridData gd = new GridData();
//        gd.horizontalAlignment = GridData.FILL;
//        gd.verticalAlignment = GridData.CENTER;
//        gd.grabExcessHorizontalSpace = true;
//        gd.grabExcessVerticalSpace = false;
//        gd.horizontalSpan = 2;
//        info.setLayoutData(gd);

        //Field label
        Label label = new Label(composite, SWT.NONE);
        label.setText(option.getLabel());
        GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.CENTER;
        gd.grabExcessHorizontalSpace = false;
        gd.grabExcessVerticalSpace = false;
        gd.widthHint =convertWidthInCharsToPixels(label.getText().length() + 4);        
        label.setLayoutData(gd);

        valueField = new Text(composite, SWT.SINGLE | SWT.BORDER);
        gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        gd.widthHint = convertWidthInCharsToPixels(TEXT_FIELD_WIDTH);
        valueField.setLayoutData(gd);
        valueField.addModifyListener(new ModifyListener() {
            String old="";
            @Override
			public void modifyText(ModifyEvent e) {
                if(old.equals(e.data)) {
                    return;
                }
                firePropertyChange(old,(String) e.data);
                old=""+e.data;
            }
        });
    }

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#setValue(java.lang.String)
     */
    @Override
	public void setValue(String value) {
        valueField.setText(value);
    }

    
    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#getValue()
     */
    @Override
	public String getValue() {      
        return valueField.getText();
    }

}



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

import org.cs3.pl.common.Option;
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

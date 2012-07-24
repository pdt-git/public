/* $LICENSE_MSG$ */

package org.cs3.prolog.ui.util.preferences;

import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyRadioGroupFieldEditor extends RadioGroupFieldEditor implements FieldEditorForStructuredPreferencePage {
	
	public MyRadioGroupFieldEditor(String name, String labelText, int numColumns, String[][] labelAndValues, Composite parent) {
		super(name,labelText,numColumns,labelAndValues,parent,true);
    	this.parent = parent;
    }
    
	@Override
	public void adjustColumns(int numColumns) {
		adjustForNumColumns(numColumns);
	}

    private Composite parent;
	
	@Override
	public Composite getParent() {
		return parent;
	}
	
}


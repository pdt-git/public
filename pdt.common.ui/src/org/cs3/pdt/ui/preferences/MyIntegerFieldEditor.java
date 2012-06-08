package org.cs3.pdt.ui.preferences;

import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyIntegerFieldEditor extends IntegerFieldEditor implements FieldEditorForStructuredPreferencePage {

    private Composite parent;

	public MyIntegerFieldEditor(String name, String labelText, Composite parent) {
    	super(name, labelText, parent);
    	this.parent = parent;
    }
    
    public MyIntegerFieldEditor(String name, String labelText, Composite parent, int textLimit) {
    	super(name, labelText, parent, textLimit);
    	this.parent = parent;
    }

	@Override
	public void adjustColumns(int numColumns) {
		adjustForNumColumns(numColumns);
	}

	@Override
	public Composite getParent() {
		return parent;
	}

}

package org.cs3.pdt.ui.preferences;

import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyStringFieldEditor extends StringFieldEditor implements FieldEditorForStructuredPreferencePage {

    private Composite parent;

	public MyStringFieldEditor(String name, String labelText, int width, int strategy, Composite parent) {
        super(name, labelText, width, strategy, parent);
        this.parent = parent;
    }

    public MyStringFieldEditor(String name, String labelText, int width, Composite parent) {
        super(name, labelText, width, parent);
        this.parent = parent;
    }

    public MyStringFieldEditor(String name, String labelText, Composite parent) {
    	super(name, labelText, parent);
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

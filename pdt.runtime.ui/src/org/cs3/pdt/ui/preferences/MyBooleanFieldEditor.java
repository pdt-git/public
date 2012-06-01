package org.cs3.pdt.ui.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyBooleanFieldEditor extends BooleanFieldEditor implements FieldEditorForStructuredPreferencePage{
	
	public MyBooleanFieldEditor(String name, String labelText, int style, Composite parent) {
		super(name, labelText, style, parent);
    	this.parent = parent;
	}
	
	public MyBooleanFieldEditor(String name, String label, Composite parent) {
		super(name, label, parent);
    	this.parent = parent;
	}
	
	public void adjustColumns(int numColumns) {
		adjustForNumColumns(numColumns);
	}
	
    private Composite parent;
	
	@Override
	public Composite getParent() {
		return parent;
	}
	
}

package org.cs3.pdt.ui.preferences;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class MyColorFieldEditor extends ColorFieldEditor implements FieldEditorForStructuredPreferencePage{
	
	public MyColorFieldEditor(String name, String labelText, Composite parent) {
		super(name, labelText, parent);
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
	
	@Override
	public Label getLabelControl() {
		return super.getLabelControl();
	}
	
}
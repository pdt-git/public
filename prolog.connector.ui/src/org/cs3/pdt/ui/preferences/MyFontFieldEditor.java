package org.cs3.pdt.ui.preferences;

import org.eclipse.jface.preference.FontFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyFontFieldEditor extends FontFieldEditor implements FieldEditorForStructuredPreferencePage {
	
	public MyFontFieldEditor(String name, String labelText, String previewAreaText, Composite parent) {
    	super(name, labelText, previewAreaText, parent);
    	this.parent = parent;
    }
    
	public MyFontFieldEditor(String name, String labelText, Composite parent) {
		super(name, labelText, parent);
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

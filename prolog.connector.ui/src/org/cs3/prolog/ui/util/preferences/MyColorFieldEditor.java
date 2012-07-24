/* $LICENSE_MSG$ */

package org.cs3.prolog.ui.util.preferences;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class MyColorFieldEditor extends ColorFieldEditor implements FieldEditorForStructuredPreferencePage{
	
	public MyColorFieldEditor(String name, String labelText, Composite parent) {
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
	
	@Override
	public Label getLabelControl() {
		return super.getLabelControl();
	}
	
}


/* $LICENSE_MSG$ */

package org.cs3.prolog.ui.util.preferences;

import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyDirectoryFieldEditor extends DirectoryFieldEditor implements FieldEditorForStructuredPreferencePage {

    private Composite parent;

	public MyDirectoryFieldEditor(String name, String labelText, Composite parent) {
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


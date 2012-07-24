/* $LICENSE_MSG$ */

package org.cs3.prolog.ui.util.preferences;

import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyFileFieldEditor extends FileFieldEditor implements FieldEditorForStructuredPreferencePage {

	public MyFileFieldEditor(String name, String labelText, Composite parent) {
		super(name, labelText, parent);
		this.parent = parent;
	}

	public MyFileFieldEditor(String name, String labelText, boolean enforceAbsolute, Composite parent) {
		super(name, labelText, enforceAbsolute, parent);
		this.parent = parent;
	}

	public MyFileFieldEditor(String name, String labelText, boolean enforceAbsolute, int validationStrategy, Composite parent) {
		super(name, labelText, enforceAbsolute, validationStrategy, parent);
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


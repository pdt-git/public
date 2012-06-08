package org.cs3.pdt.ui.preferences;

import java.util.ArrayList;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

class EditorGroup {
	
	private Composite parent;
	private ArrayList<FieldEditor> editors = new ArrayList<FieldEditor>();
	private int maxColumns = 0;
	
	EditorGroup(Composite parent) {
		if (parent != null) {
			GridLayout layout = new GridLayout();
			parent.setLayout(layout);
			parent.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
		}
		this.parent = parent;
	}
	
	void addEditor(FieldEditor e) {
		if (!editors.contains(e)) {
			editors.add(e);
			maxColumns = Math.max(maxColumns, e.getNumberOfControls());
		}
	}
	
	void adjustGroupAndEditors() {
		if (parent != null) {
			((GridLayout)parent.getLayout()).numColumns = maxColumns;
		}
		for (FieldEditor e : editors) {
			if (e instanceof FieldEditorForStructuredPreferencePage) {
				((FieldEditorForStructuredPreferencePage) e).adjustColumns(maxColumns);
			}
		}
	}
	
	int getMaxColumns() {
		return maxColumns;
	}
	
}

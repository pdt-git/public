package org.cs3.pdt.ui.preferences;

import org.eclipse.swt.widgets.Composite;

interface FieldEditorForStructuredPreferencePage {

	void adjustColumns(int numColumns);
	
	Composite getParent();
	
}

/* $LICENSE_MSG$ */

package org.cs3.prolog.ui.util.preferences;

import org.eclipse.swt.widgets.Composite;

interface FieldEditorForStructuredPreferencePage {

	void adjustColumns(int numColumns);
	
	Composite getParent();
	
}


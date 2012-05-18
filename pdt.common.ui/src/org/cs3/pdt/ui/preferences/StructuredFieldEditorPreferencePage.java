package org.cs3.pdt.ui.preferences;

import java.util.HashMap;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;

public abstract class StructuredFieldEditorPreferencePage extends FieldEditorPreferencePage {

	public StructuredFieldEditorPreferencePage() {
		super();
	}

    protected StructuredFieldEditorPreferencePage(int style) {
        super(style);
    }

    protected StructuredFieldEditorPreferencePage(String title, int style) {
        super(title, style);
    }

    protected StructuredFieldEditorPreferencePage(String title, ImageDescriptor image, int style) {
        super(title, image, style);
    }
    
    private HashMap<Composite, EditorGroup> editorGroups = new HashMap<Composite, EditorGroup>();
    
    @Override
    protected void adjustGridLayout() {
    	for (EditorGroup eg : editorGroups.values()) {
    		eg.adjustGroupAndEditors();
    	}
    }

    @Override
    protected void addField(FieldEditor editor) {
    	super.addField(editor);
    	if (editor instanceof FieldEditorForStructuredPreferencePage) {
    		Composite parent = ((FieldEditorForStructuredPreferencePage) editor).getParent();
    		EditorGroup editorGroup = editorGroups.get(parent);
			if (editorGroup == null) {
				editorGroup = new EditorGroup(parent);
				editorGroups.put(parent, editorGroup);
			}
			editorGroup.addEditor(editor);
    	}
    }
}

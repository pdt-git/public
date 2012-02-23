package pdt.y.preferences;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbenchPreferencePage;

import pdt.y.main.PluginActivator;
import pdt.y.main.PreferencesUpdateListener;

public abstract class PreferencePageBase 
		extends FieldEditorPreferencePage 
		implements IWorkbenchPreferencePage, PreferencesUpdateListener {

	private List<FieldEditor> editors = new LinkedList<FieldEditor>();
	
	public PreferencePageBase() {
		super(GRID);
		setPreferenceStore(PluginActivator.getDefault().getPreferenceStore());
		PluginActivator.getDefault().addPreferencesUpdateListener(this);
	}
	
	@Override
	public boolean performOk() {
		boolean res = super.performOk();
		if (res) {
			PluginActivator.getDefault().preferencesUpdated();
		}
		return res;
	}
	
	@Override
	protected void addField(FieldEditor editor) {
		editors.add(editor);
		super.addField(editor);
	}
	
	@Override
	public void preferencesUpdated() {
		for (FieldEditor e : editors) {
			if (e != null) {
				e.load();
			}
		}
	}
	
	@Override
	public void dispose() {
		PluginActivator.getDefault().removePreferencesUpdateListener(this);
		super.dispose();
	}
}

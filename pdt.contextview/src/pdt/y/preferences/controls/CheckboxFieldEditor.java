package pdt.y.preferences.controls;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.swt.widgets.Composite;

import pdt.y.utils.GenericEventListener;



public class CheckboxFieldEditor extends BooleanFieldEditor {

	List<GenericEventListener<Boolean>> listeners = new LinkedList<GenericEventListener<Boolean>>();
	
	public CheckboxFieldEditor(String name, String label, Composite parent) {
		super(name, label, parent);
	}
	
	public void addListener(GenericEventListener<Boolean> listener) {
		listeners.add(listener);
	}
	
	public void removeListener(CheckboxFieldEditor listener) {
		listeners.remove(listener);
	}

	@Override
	protected void valueChanged(boolean oldValue, boolean newValue) {
		super.valueChanged(oldValue, newValue);
		
		for (GenericEventListener<Boolean> l : listeners) {
			l.valueChanged(oldValue, newValue);
		}
	}
}

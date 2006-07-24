package org.cs3.pdt.internal.views;

import org.cs3.pl.cterm.CTerm;
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.PropertyDescriptor;

public class CTermPropertySource implements IPropertySource {

	private CTerm term;

	public CTermPropertySource(CTerm term) {
		this.term=term;
	}

	public Object getEditableValue() {
		return this;
	}

	public IPropertyDescriptor[] getPropertyDescriptors() {
		String[] keys = term.getAnnotationKeys();
		PropertyDescriptor[] descriptors = new PropertyDescriptor[keys.length];
		for (int i = 0; i < keys.length; i++) {
			String key = keys[i];
			descriptors[i]=new PropertyDescriptor(key,key);
			descriptors[i].setAlwaysIncompatible(true);
		}
		return descriptors;
	}

	public Object getPropertyValue(Object id) {

		return term.getAnotation((String) id);
	}

	public boolean isPropertySet(Object id) {
		return false;
	}

	public void resetPropertyValue(Object id) {
		;
	}

	public void setPropertyValue(Object id, Object value) {
		;
	}

}

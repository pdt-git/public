package org.cs3.pdt.internal.views;

import java.util.Map;

import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.PropertyDescriptor;

public class SimplePropertySource implements IPropertySource {

	private Map properties;

	public SimplePropertySource(Map properties) {
		this.properties=properties;
	}

	public Object getEditableValue() {
		return this;
	}

	public IPropertyDescriptor[] getPropertyDescriptors() {
		String[] keys = (String[]) properties.keySet().toArray(new String[properties.size()]);
		PropertyDescriptor[] descriptors = new PropertyDescriptor[keys.length];
		for (int i = 0; i < keys.length; i++) {
			String key = keys[i];
			descriptors[i]=new PropertyDescriptor(key,key);
			descriptors[i].setAlwaysIncompatible(true);
		}
		return descriptors;
	}

	public Object getPropertyValue(Object id) {

		return properties.get(id);
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

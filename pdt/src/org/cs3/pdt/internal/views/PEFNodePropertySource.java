package org.cs3.pdt.internal.views;

import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.PropertyDescriptor;

public class PEFNodePropertySource implements IPropertySource {

	private PEFNode node;

	public PEFNodePropertySource(PEFNode node) {
		this.node = node;
	}

	@Override
	public Object getEditableValue() {

		return this;
	}

	@Override
	public IPropertyDescriptor[] getPropertyDescriptors() {

		PropertyDescriptor[] r = new PropertyDescriptor[] {
				new PropertyDescriptor("id", "PEF Id"),
				new PropertyDescriptor("label", "Label"),
				new PropertyDescriptor("type", "PEF Type"),
				new PropertyDescriptor("start", "Start Offset"),
				new PropertyDescriptor("end", "End Offset"),
				new PropertyDescriptor("tags", "Tags") };
		for (int i = 0; i < r.length; i++) {
			r[i].setAlwaysIncompatible(true);
		}
		return r;

	}

	@Override
	public Object getPropertyValue(Object id) {
		if ("label".equals(id)) {
			return node.getLabel();
		}
		if ("type".equals(id)) {
			return node.getType();
		}
		if ("start".equals(id)) {
			return node.getStartPosition();
		}
		if ("end".equals(id)) {
			return node.getEndPosition();
		}
		if ("tags".equals(id)) {
			return node.getTags();
		}
		if ("id".equals(id)) {
			return node.getId();
		}
		return null;
	}

	@Override
	public boolean isPropertySet(Object id) {
		
		return false;
	}

	@Override
	public void resetPropertyValue(Object id) {

	}

	@Override
	public void setPropertyValue(Object id, Object value) {

	}

}

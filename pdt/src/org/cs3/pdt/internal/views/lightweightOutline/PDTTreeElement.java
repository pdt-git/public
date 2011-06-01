package org.cs3.pdt.internal.views.lightweightOutline;

public interface PDTTreeElement {
	public boolean hasChildren();
	public Object[] getChildren();
	public String getLabel();
}

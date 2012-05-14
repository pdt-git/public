package org.cs3.pdt.internal.structureElements;

public interface PrologTreeElement {
	
	public boolean hasChildren();
	
	public Object[] getChildren();
	
	public String getLabel();
}

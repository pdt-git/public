package org.cs3.pdt.internal.structureElements;

public interface PDTSearchTreeElement extends PDTTreeElement{
	
	void addMatch(PDTMatch match);
	
	void removeMatch(PDTMatch match);
	
	Object getParent();
	
}

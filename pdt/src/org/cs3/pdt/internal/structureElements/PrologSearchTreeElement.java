package org.cs3.pdt.internal.structureElements;

public interface PrologSearchTreeElement extends PrologTreeElement{
	
	void addMatch(PrologMatch match);
	
	void removeMatch(PrologMatch match);
	
}

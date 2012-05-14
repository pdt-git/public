package org.cs3.pdt.internal.structureElements;

public class SearchMatchElement implements PrologSearchTreeElement {

	private PrologMatch match;
	private Object parent;
	
	public SearchMatchElement() {
	}
	
	public void setMatch(PrologMatch match) {
		this.match = match;
	}
	
	public void setParent(Object parent) {
		this.parent = parent;
	}
	
	@Override
	public boolean hasChildren() {
		return false;
	}

	@Override
	public Object[] getChildren() {
		return new Object[0];
	}

	@Override
	public String getLabel() {
		return match.getLabel();
	}

	public PrologMatch getMatch() {
		return match;
	}
	
	@Override
	public void removeMatch(PrologMatch match) {
		if (match == this.match) {
			this.match = null;
		}
	}

	@Override
	public void addMatch(PrologMatch match) {
		setMatch(match);
	}

	@Override
	public Object getParent() {
		if (parent == null) {
			if (match == null) {
				return null;
			} else {
				return match.getFile();
			}
		} else {
			return parent;
		}
	}

}

package org.cs3.pdt.internal.structureElements;


public class PredicateOccuranceElement implements PrologTreeElement{
	private String label;
	private int line;
	private String type;
	private PrologTreeElement parent;
	private String file;
	
	public PredicateOccuranceElement(String label, String file, int line, String type, PrologTreeElement parent) {
		this.label = label;
		this.line = line;
		this.type = type;
		this.file = file;
		this.parent = parent;
	}
	
	public int getLine() {
		return line;
	}
	
	public String getType() {
		return type;
	}
	
	public PrologTreeElement getParent() {
		return parent;
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
		return label;
	}
	
	public String getFile() {
		return file;
	}

	@Override
	public int hashCode() {
		return (file + label).hashCode();
	}
	
	@Override
	public boolean equals(Object object) {
		if (object == null || !(object instanceof PredicateOccuranceElement)) {
			return false;
		} else {
			PredicateOccuranceElement other = (PredicateOccuranceElement) object;
			return (file.equals(other.file) && label.equals(other.label) && line == other.line);
		}
	}

}

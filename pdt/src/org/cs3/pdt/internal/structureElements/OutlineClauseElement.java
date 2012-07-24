/* $LICENSE_MSG$ */

package org.cs3.pdt.internal.structureElements;



public class OutlineClauseElement implements PrologOutlineTreeElement{
	
	private static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];
	private String label;
//	private int line;
//	private String type;
	private Object parent;
//	private String file;
	private PrologClause clause;
	
//	public OutlineClauseElement(String label, String file, int line, String type, PrologTreeElement parent) {
//		this.label = label;
//		this.line = line;
//		this.type = type;
//		this.file = file;
//		this.parent = parent;
//	}
	
	public OutlineClauseElement(Object parent, PrologClause clause) {
		this.parent = parent;
		this.clause = clause;
		this.label = calculateOccuranceLabel();
	}

	public int getLine() {
		return clause.getLine();
	}
	
	public String getType() {
		return clause.getType();
	}
	
	@Override
	public Object getParent() {
		return parent;
	}
	@Override
	public boolean hasChildren() {
		return false;
	}

	@Override
	public Object[] getChildren() {
		return EMPTY_OBJECT_ARRAY;
	}

	@Override
	public String getLabel() {
		return label;
	}
	
	public String getFile() {
		return clause.getOccuranceFile();
	}

	@Override
	public int hashCode() {
		return (clause.getOccuranceFile() + label).hashCode();
	}
	
	@Override
	public boolean equals(Object object) {
		if (object == null || !(object instanceof OutlineClauseElement)) {
			return false;
		} else {
			OutlineClauseElement other = (OutlineClauseElement) object;
			return (getFile().equals(other.getFile())
					&& label.equals(other.label)
					&& getLine() == other.getLine());
		}
	}

	@Override
	public void addClause(PrologClause clause) {
		this.clause = clause;
	}

	private String calculateOccuranceLabel() {
		StringBuffer occuranceLabel = new StringBuffer("Line: ");
		occuranceLabel.append(Integer.toString(clause.getLine()));
		occuranceLabel.append(" (");
		occuranceLabel.append(clause.getType());
		occuranceLabel.append(")");
		return occuranceLabel.toString();
	}

	public String getFunctor() {
		return clause.getFunctor();
	}

	public int getArity() {
		return clause.getArity();
	}
}


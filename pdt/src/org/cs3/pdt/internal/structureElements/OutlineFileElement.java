/* $LICENSE_MSG$ */

package org.cs3.pdt.internal.structureElements;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.metadata.PrologSourceLocation;

public class OutlineFileElement extends PrologSourceLocation implements PrologOutlineTreeElement, Comparable<OutlineFileElement> {

	private String fullFileName;
	private String fileName;
	
	private List<OutlineClauseElement> clauses = new ArrayList<OutlineClauseElement>();
	private Object parent;
	
	public OutlineFileElement(Object parent, String file) {
		super(file, 1);
		fullFileName = file;
		fileName = getFileName(file);
		this.parent = parent;
	}

	@Override
	public boolean hasChildren() {
		return !clauses.isEmpty();
	}

	@Override
	public Object[] getChildren() {
		return clauses.toArray();
	}

	@Override
	public String getLabel() {
		return fileName;
	}
	
	private String getFileName(String fullPathOfFile) {
		return new File(fullPathOfFile).getName();
	}

	public int numberOfClauses() {
		return clauses.size();
	}
	
	@Override
	public int hashCode() {
		return fullFileName.hashCode();
	}
	
	@Override
	public boolean equals(Object object) {
		if (object == null || !(object instanceof OutlineFileElement)) {
			return false;
		} else {
			return fullFileName.equals(((OutlineFileElement) object).fullFileName); 
		}
	}

	@Override
	public int compareTo(OutlineFileElement o) {
		return fullFileName.compareTo(o.fullFileName);
	}

	@Override
	public Object getParent() {
		return parent;
	}

	@Override
	public void addClause(PrologClause clause) {
		OutlineClauseElement clauseElement = new OutlineClauseElement(this, clause);
		clauses.add(clauseElement);
	}
	
	public int getFirstLine() {
		if (clauses.isEmpty()) {
			return getLine();
		} else {
			return clauses.get(0).getLine();
		}
	}
}


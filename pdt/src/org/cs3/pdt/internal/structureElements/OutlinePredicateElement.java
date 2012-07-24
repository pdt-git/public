/* $LICENSE_MSG$ */

package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.cs3.pdt.metadata.Predicate;

public class OutlinePredicateElement extends Predicate implements PrologOutlineTreeElement{
	private static final long serialVersionUID = 2577159022013132807L;
	
	private String fileName;
	private List<OutlineClauseElement> clauses = new ArrayList<OutlineClauseElement>();
	private HashMap<String, OutlineFileElement> otherFiles = new HashMap<String, OutlineFileElement>();
	private List<Object> allChildren = new ArrayList<Object>();

	private Object parent;
	
	public OutlinePredicateElement(Object parent, String module, String functor, int arity, List<String> properties, String fileName){
		super(module, functor, arity, properties);
		this.fileName = fileName;
		this.parent = parent;
	}
	
	public int getLine() {
		int line = -1;
		int defLine = -1;
		for (OutlineClauseElement occurence : clauses) {
			int occuranceLine = occurence.getLine();
			String type = occurence.getType();
			if ((line < 0) || occuranceLine < line) {
				line = occuranceLine;
			}
			if ((!type.equals("declaration")) && ((defLine < 0) || (occuranceLine < defLine))) {
				defLine = occuranceLine;
			}
		}
		if (defLine >= 0) {
			return defLine;
		}
		return line;
	}
	
	public int numberOfClauses() {
		List<String> properties = getProperties();
		for (String property : properties) {
			if (property.contains("number_of_clauses")) {
				String numberString = property.substring(18, property.length()-1);
				return Integer.parseInt(numberString);
			} 
		}
		
		int numClauses = clauses.size();
		for (OutlineFileElement outlineFileElement : otherFiles.values()) {
			numClauses += outlineFileElement.numberOfClauses();
		}
		return numClauses;
	}
	
	public String getFileName() {
		return fileName;
	}

	@Override
	public boolean hasChildren() {
		return !allChildren.isEmpty();
	}

	@Override
	public Object[] getChildren() {
		return allChildren.toArray();
	}

	@Override
	public String getLabel() {
		StringBuffer label = new StringBuffer(getFunctor());
		label.append("/");
		label.append(getArity());
		label.append(" (");
		int numberOfClauses = numberOfClauses();
		label.append(numberOfClauses);
		if (numberOfClauses != 1) {
			label.append(" clauses)");
		} else {
			label.append(" clause)");
		}
		return label.toString();
	}
	
	@Override
	public boolean equals(Object object) {
		if (object == null || !(object instanceof OutlinePredicateElement)) {
			return false;
		} else {
			return super.equals(object);
		}
	}
	
	@Override
	public Object getParent() {
		return parent;
	}

	@Override
	public void addClause(PrologClause clause) {
		if (clause.isFromOtherFile()) {
			String occuranceFile = clause.getOccuranceFile();
			OutlineFileElement outlineFileElement = otherFiles.get(occuranceFile);
			if (outlineFileElement == null) {
				outlineFileElement = new OutlineFileElement(this, occuranceFile);
				otherFiles.put(occuranceFile, outlineFileElement);
				allChildren.add(outlineFileElement);
			}
			outlineFileElement.addClause(clause);
		} else {
			OutlineClauseElement clauseElement = new OutlineClauseElement(this, clause);
			clauses.add(clauseElement);
			allChildren.add(clauseElement);
		}
	}
	
}



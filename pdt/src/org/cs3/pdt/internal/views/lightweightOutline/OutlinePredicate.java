package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.ArrayList;
import java.util.List;

import org.cs3.pl.metadata.Predicate;

public class OutlinePredicate extends Predicate implements PDTTreeElement{
	private static final long serialVersionUID = 2577159022013132807L;
	
	private List<PredicateOccuranceElement> occurences = new ArrayList<PredicateOccuranceElement>();
	
	public OutlinePredicate(String module, String functor, int arity, List<String> properties){
		super(module, functor, arity, properties);
	}
	
	public void addOccurance(PredicateOccuranceElement occurance) {
		occurences.add(occurance);
	}

	public int getLine() {
		int line = -1;
		int defLine = -1;
		for (PredicateOccuranceElement occurence : occurences) {
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
			else if (property.contains("clauses")) {
				String numberString = property.substring(8, property.length()-1);
				return Integer.parseInt(numberString);
			}
		}
		return 0;
	}
	

	@Override
	public boolean hasChildren() {
		return !occurences.isEmpty();
	}

	@Override
	public Object[] getChildren() {
		return occurences.toArray();
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
	
}


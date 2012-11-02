package org.cs3.pdt.common.structureElements;

import java.util.List;

import org.eclipse.search.ui.text.Match;

public class PredicateMatch extends Match {

	private String module;
	private String name;
	private int arity;
	private String visibility;
	private String declOrDef;
	private List<String> properties;
	
	public PredicateMatch(SearchPredicateElement searchPredicateElement, String visibility, String module, String name, int arity, List<String> properties, String declOrDef) {
		super(searchPredicateElement, UNIT_LINE, 0, 0);
		this.declOrDef = declOrDef;
		this.visibility = visibility;
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.properties = properties;
	}
	
	public String getModule() {
		return module;
	}
	
	public String getName() {
		return name;
	}
	
	public int getArity() {
		return arity;
	}
	
	public List<String> getProperties() {
		return properties;
	}
	
	public String getDeclOrDef() {
		return declOrDef;
	}
	
	public String getVisibility() {
		return visibility;
	}
	
}

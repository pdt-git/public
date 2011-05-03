package org.cs3.pdt.internal.editors;

import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.internal.views.lightweightOutline.PrologPredicate;

public class PrologSourceFileModel {
	
	public PrologSourceFileModel(List<PrologPredicate> predicates, String file) {
		super();
		this.predicates = predicates;
		this.fileName = file;
	}

	private List<PrologPredicate> predicates= new ArrayList<PrologPredicate>();
	private String  fileName;
	
	public String getFileName() {
		return fileName;
	}

	public void setFile(String file) {
		this.fileName = file;
	}

	public List<PrologPredicate> getPredicates() {
		return predicates;
	}

	public void setPredicates(List<PrologPredicate> predicates) {
		this.predicates = predicates;
	}
	
	
}

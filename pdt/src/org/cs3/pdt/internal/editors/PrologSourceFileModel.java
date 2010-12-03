package org.cs3.pdt.internal.editors;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;

public class PrologSourceFileModel {
	
	public PrologSourceFileModel(List<PrologPredicate> predicates, IFile file) {
		super();
		this.predicates = predicates;
		this.file = file;
	}

	private List<PrologPredicate> predicates= new ArrayList<PrologPredicate>();
	private IFile file;
	
	public IFile getFile() {
		return file;
	}

	public void setFile(IFile file) {
		this.file = file;
	}

	public List<PrologPredicate> getPredicates() {
		return predicates;
	}

	public void setPredicates(List<PrologPredicate> predicates) {
		this.predicates = predicates;
	}
	
	
}

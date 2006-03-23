package org.cs3.pdt.internal.views;

import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.SourceLocation;
import org.eclipse.core.resources.IFile;

public class ClauseNode implements Clause{
	CTerm term;
	IFile file;
	private SourceLocation loc;
	PredicateNode predicate;
	public ClauseNode(CTerm term, IFile file) {		
		this.term = term;
		this.file = file;
		CCompound posterm = (CCompound) term.getAnotation("position");
		int from = ((CInteger)posterm.getArgument(0)).getIntValue();
		int to = ((CInteger)posterm.getArgument(1)).getIntValue();
		loc = new SourceLocation(file.getFullPath().toString(),true,false);
		loc.offset=from;
		loc.endOffset=to;
		CCompound anno = (CCompound) term.getAnotation("clause_of");
		String module = anno.getArgument(0).getFunctorValue();
		anno=(CCompound) anno.getArgument(1);
		String name = anno.getArgument(0).getFunctorValue();
		int arity = ((CInteger)anno.getArgument(1)).getIntValue();
		predicate = new PredicateNode(module,name,arity);
	}
	public SourceLocation getSourceLocation() {	
		return loc;
	}
	public String getName() {
		
		return predicate.getName();
	}
	public int getLength() {

		return loc.endOffset-loc.offset;
	}
	public boolean isPublic() {
		return predicate.isPublic();
	}
	public String getSignature() {
		return getPredicate().getSignature();
	}
	public int getPosition() {
		return loc.offset;
	}
	public int getArity() {
		return getPredicate().getArity();
	}
	public boolean isDynamic() {
		return getPredicate().isDynamic();
	}
	public boolean isMultifile() {
		return getPredicate().isMultifile();
	}
	public Predicate getPredicate() {

		return predicate;
	}
	public int compareTo(Object arg0) {
		if(!(arg0 instanceof Clause)){
			return -1;
		}
		Clause other = (Clause) arg0;
		return getSourceLocation().compareTo(other.getSourceLocation());
	}
	
	public boolean equals(Object obj) {
		return compareTo(obj)==0;
	}
	public int hashCode() {
		return getSourceLocation().hashCode();
	}
}

package org.cs3.pdt.internal.views;

import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Directive;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.SourceLocation;
import org.eclipse.core.resources.IFile;

public class DirectiveNode implements Directive{
	CTerm term;
	
	private SourceLocation loc;
	private String contextModule;

	private GoalNode goal;
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
	public DirectiveNode(IFile file, String contextModule,CTerm term){
		this.term = term;
		this.contextModule=contextModule;
		CCompound posterm = (CCompound) term.getAnotation("position");
		int from = ((CInteger)posterm.getArgument(0)).getIntValue();
		int to = ((CInteger)posterm.getArgument(1)).getIntValue();
		loc = new SourceLocation(file.getFullPath().toString(),true,false);
		loc.offset=from;
		loc.endOffset=to;
		goal = new GoalNode(contextModule,((CCompound)term).getArgument(0));
	}
	
	public SourceLocation getSourceLocation() {		
		return loc;
	}

	public Goal getBody() {
		
		
		return goal;
	}
}

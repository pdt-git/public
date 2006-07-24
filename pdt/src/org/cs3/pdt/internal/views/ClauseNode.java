/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.internal.views;

import java.io.File;
import java.io.IOException;

import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.SourceLocation;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.views.properties.IPropertySource;

public class ClauseNode implements Clause,IAdaptable{
	CTerm term;
	
	private SourceLocation loc;
	PredicateNode predicate;
	public ClauseNode(CTerm term, IFile file) throws IOException {		
		this(term,file.getLocation().toFile());
	}
	public ClauseNode(CTerm term, File file) throws IOException {		
		this.term = term;
	
		CCompound posterm = (CCompound) term.getAnotation("position");
		int from = ((CInteger)posterm.getArgument(0)).getIntValue();
		int to = ((CInteger)posterm.getArgument(1)).getIntValue();
		loc = new SourceLocation(file,false);
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
	
	public CTerm getHeadTerm(){
		if(term.getFunctorValue().equals(":-") && term.getArity()==2){
			return ((CCompound)term).getArgument(0);
		}
		return term;
	}
	public Object getAdapter(Class adapter) {
		if(IPropertySource.class.isAssignableFrom(adapter)){
			return new CTermPropertySource(term);
		}
		return null;
	}
}

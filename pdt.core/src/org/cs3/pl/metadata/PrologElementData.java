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

package org.cs3.pl.metadata;

import java.io.Serializable;
import java.util.Comparator;


/**
 * a tuple describing a logical prolog element like a predicate or a module..

 */
public class PrologElementData implements Serializable, Comparable<PrologElementData>{

	/**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    
	protected String module;
	protected String label;
	protected int arity;
	
	protected boolean dynamic;
	protected boolean multifile;
	protected boolean pub;

	protected String modulePrefix="";

	protected SourceLocation knownDefinition = null;

	protected boolean m_hasKnownDefinition;

	protected boolean isKnown;
	
	
	/**
	 * @param name
	 * @param arity
	 */
	protected PrologElementData(String module,String label, int arity,boolean pub, boolean dynamic, boolean multifile, SourceLocation knownDefinition) {
		this.pub = pub;
		this.label = label;
		this.arity = arity;
		this.dynamic = dynamic;
		this.multifile =multifile;
		this.module=module;		
		this.isKnown=true;
		this.knownDefinition=knownDefinition;
		this.m_hasKnownDefinition=true;
	}


	/**
	 * @param name
	 * @param arity
	 */
	protected PrologElementData(String module,String label, int arity,boolean pub, boolean dynamic, boolean multifile) {
		this.pub = pub;
		this.label = label;
		this.arity = arity;
		this.dynamic = dynamic;
		this.multifile =multifile;
		this.module=module;		
		this.isKnown=true;
		this.m_hasKnownDefinition=false;
	}
	
	@Override
	public int hashCode() {
		return getSignature().hashCode();
	}
	@Override
	public boolean equals(Object obj) {
		if(obj==null){
			return false;
		}
		if(obj instanceof PrologElementData){
			return ((PrologElementData)obj).getSignature().equals(getSignature());
		}
		return super.equals(obj);
	}

	/**
	 * Creates a PrologElementData Entity. This class is a container for
	 * Prolog elements like facts, clauses or modules. 
	 * 
	 * @param elementName
	 * @param arity if arity is -1 the element is a module.
	 */
	protected PrologElementData(String module,String elementName, int arity) {
		this.module=module;
		label = elementName;
		this.arity = arity;
		this.isKnown=false;
		this.m_hasKnownDefinition=false;
	}


	/**
	 * Returns the signature of the predicate:
	 * module:name/arity.
	 * 
	 * @return
	 */
	public String getSignature() {
		if(arity == -1)
			return label + " (module)";
		return module+":"+label + "/" + arity;
	}

	@Override
	public String toString() {
		return getSignature();
	}



	public int getArity() {
		return arity;
	}

	

	public boolean isDynamic() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return dynamic;
	}

	

	public boolean isMultifile() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return multifile;
	}




	public String getName() {
		return label;
	}
		
	static public String getPredicateName(String line) {
		line = line.substring(0, line.indexOf('('));
		return line;
	}
	
	

	public boolean belongsToSamePredicate(Clause data){
		if (data.getArity() == arity &&
			data.getName().equals(label))
			return true;
		else
			return false;
	}
	/**
	 * @return Returns the length.
	 */
	public int getLength() {
		return knownDefinition==null?-1:knownDefinition.getEndOffset()-knownDefinition.getOffset();
	}
	
	
	

	static public Comparator<PrologElementData> getComparator() {
		return new Comparator<PrologElementData>() {

			@Override
			public int compare(PrologElementData arg0, PrologElementData arg1) {
				return arg0.getSignature().compareTo(
						arg1.getSignature());
			}
			
		};
	}
	
	@Override
	public int compareTo(PrologElementData arg0) {
		return getSignature().compareTo(((Predicate)arg0).getSignature());
	}
	

/**
 * @return Returns the pub.
 */
public boolean isPublic() {
	if(!isKnown){
		throw new UnsupportedOperationException("Not enough information.");
	}
	return pub;
}


/**
 * @return checks if this prolog element is a module.
 */
public boolean isModule() {
	return arity == -1;
}





public SourceLocation getSourceLocation() {
	return knownDefinition;
}


public boolean hasKnownDefinition() {
	return m_hasKnownDefinition;
}


public boolean isKnown() {
	return isKnown;
}


public String getModule() {
	return module;
}







}

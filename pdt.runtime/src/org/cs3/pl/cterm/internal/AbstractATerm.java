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

package org.cs3.pl.cterm.internal;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.cs3.pdt.runtime.PLUtil;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.internal.parser.SimpleNode;

public abstract class AbstractATerm implements CTerm {

	
	private ATermFactory factory;
	protected CTerm term;
	Map annotation;
	protected boolean anotated;
	
	public CTerm getAnotation(String functor) {	
		if(annotation==null){
			annotation = new HashMap();
			if(anotated){
				processAnotations();
			}
		}
		return (CTerm) annotation.get(functor);
	}
	
	public boolean hasAnnotation(String functor) {	
		if(annotation==null){
			annotation = new HashMap();
			if(anotated){
				processAnotations();
			}
		}
		return  annotation.containsKey(functor);
	}
	
	private void processAnotations_bak() {
		CCompound aterm =(CCompound) this.term;
		CTerm current = aterm.getArgument(0);
		
		
		
		while(current.getFunctorValue().equals(".")
				&& current.getArity()==2){
			CTerm term = ((CCompound)current).getArgument(0);
			processAnotation(term);
			current=((CCompound)current).getArgument(1);
		}
		
	}
	
	private void processAnotations() {
		CCompound aterm =(CCompound) this.term;
		CTerm current = aterm.getArgument(0);
		
		Iterator it = PLUtil.rbtreeIterateNodes(current);
		while(it.hasNext()){
			CTerm node = (CTerm) it.next();
			processAnotation(node);
		}
		
		
	}
	private void processAnotation_bak(CTerm term) {
		String key = term.getFunctorValue();
		CTerm value = null;
		if( term instanceof CCompound){
			CCompound comp = (CCompound) term;
			value=comp.getArgument(0);
			if(comp.getArity()>1){
				throw new IllegalArgumentException("Illegal Annotation foramt:"+term);
			}
		}
		annotation.put(key,value);
	}
	private void processAnotation(CTerm term) {
		CCompound node = (CCompound) term;
		annotation.put(node.getArgument(1).getFunctorValue(),node.getArgument(2));
	}

	public AbstractATerm(ATermFactory factory, CTerm aterm) {
		this.term=aterm;
		this.factory=factory;
		this.anotated = isATerm(aterm);
	}

	public static boolean isATerm(CTerm aterm) {
		return aterm.getArity()==2&& "aterm".equals(aterm.getFunctorValue());
	}

	
	
	public String getFunctorValue() {
		if(anotated){
			return unwrapOutermostATerm().getFunctorValue();
		}
		return term.getFunctorValue();
	}

	private CTerm unwrapOutermostATerm() {
		return ((CCompound)term).getArgument(1);
	}

	public static CTerm unwrapOutermostATerm( CTerm aterm) {
		return ((CCompound)aterm).getArgument(1);
	}
	

	public CTermFactory getFactory() {
		
		return this.factory;
	}


	public String getFunctorImage() {
		if(anotated){
			return unwrapOutermostATerm().getFunctorImage();
		}
		return term.getFunctorImage();
	}



	public int getArity() {	
		if(anotated){
			return unwrapOutermostATerm().getArity();
		}
		return term.getArity();

	}
	
	

}

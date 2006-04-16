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

import org.cs3.pl.cterm.CAtom;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CFloat;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CNil;
import org.cs3.pl.cterm.CString;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.CVariable;

public class ATermFactory implements CTermFactory {

	public CTerm createCTerm(Object data) {
		if(data instanceof CTerm){
			return create((CTerm)data);
		}		
		return create(new ParserCTermFactory().createCTerm(data));
	}

	
	private  class _Atom extends AbstractATerm implements CAtom{

		public _Atom(CTerm term) {
			super(ATermFactory.this, term);
		}
		
	}
	
	private  class _String extends AbstractATerm implements CString{

		public _String(CTerm term) {
			super(ATermFactory.this, term);
		}
		
	}
	
	private  class _Nil extends AbstractATerm implements CNil{

		public _Nil(CTerm term) {
			super(ATermFactory.this, term);
		}
		
	}
	
	private  class _Variable extends AbstractATerm implements CVariable{

		public _Variable(CTerm node) {
			super(ATermFactory.this, node);
		}

		public String getVariableId() {
			return getFunctorValue();
		}
		
	}
	
	private  class _Float extends AbstractATerm implements CFloat{

		public _Float(CTerm node) {
			super(ATermFactory.this, node);
		
		}

		public double getDoubleValue() {			
			return Double.parseDouble(getFunctorValue());
		}

		
		
	}
	private  class _Integer extends AbstractATerm implements CInteger{

		public _Integer(CTerm node) {
			super(ATermFactory.this, node);
		
		}

		public int getIntValue() {		
			return Integer.parseInt(getFunctorValue());
		}
		
	}
	
	private  class _Compound extends AbstractATerm implements CCompound{

		private CTerm[] args;

		public _Compound(CTerm node) {
			super(ATermFactory.this, node);
			args = new CTerm[getArity()]; 
		}

		public CTerm getArgument(int i) {
			if(args[i]==null){
				if(anotated){
					args[i]=create(((CCompound)((CCompound)term).getArgument(1)).getArgument(i));
				}
				else{
					args[i]=create(((CCompound)term).getArgument(i));
				}
			}
			return args[i];
		}
		
		
	}
	private  CTerm create(CTerm root) {
		CTerm wrapped;
		if(AbstractATerm.isATerm(root)){
			wrapped=AbstractATerm.unwrapOutermostATerm(root);
		}
		else{
			wrapped=root;
		}
		if(wrapped instanceof CAtom){
			return new _Atom(root);
		} 
		if(wrapped instanceof CString){
			return new _String(root);
		}
		if(wrapped instanceof CVariable){
			return new _Variable( root);
		} 
		if(wrapped instanceof CCompound){
			return new _Compound(root);
		} 
		if(wrapped instanceof CInteger){
			return new _Integer( root);
		}		
		if(wrapped instanceof CFloat){
			return new _Float(root);
		}
		if(wrapped instanceof CNil){
			return new _Nil(root);
		}
		throw new IllegalArgumentException("bad cterm type: "+root.getClass().getName());
	}

}

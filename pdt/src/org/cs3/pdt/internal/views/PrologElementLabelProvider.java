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

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CVariable;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Directive;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

public class PrologElementLabelProvider implements ILabelProvider{

	
	

	public Object[] getChildren(Object o) {
		/*
		 * FIXME right now there is no sufficiently efficient way to implement
		 * this direction on a per-element basis. So tree-structured views need
		 * to use their own content provider. See PrologElementContentProvier
		 * --lu
		 */
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
	 */
	public ImageDescriptor getImageDescriptor(Object object) {
		
		if (object instanceof String) {// FIXME: need a module type --lu
			return ImageRepository
					.getImageDescriptor(ImageRepository.PE_MODULE);
		} else if (object instanceof Predicate) {
			Predicate p = (Predicate) object;
			boolean exported = p.isPublic();
			return ImageRepository
					.getImageDescriptor(exported ? ImageRepository.PE_PUBLIC
							: ImageRepository.PE_HIDDEN);
		}
		if (object instanceof Clause) {
			return ImageRepository
					.getImageDescriptor(ImageRepository.PE_CLAUSE);
		}

		String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(
				imageKey);

	}

	public String getLabel(Object o) {
	
		if (o instanceof Predicate) {
			Predicate p = (Predicate) o;
			return p.getName() + "/" + p.getArity();
		} else if (o instanceof ClauseNode) {
			ClauseNode c = (ClauseNode) o;		
			return c.getProperty("label");
		} else if (o instanceof Directive){
			DirectiveNode d = (DirectiveNode) o;			
			return d.getProperty("label");
		} else if (o instanceof CTermNode){
			CTermNode t = (CTermNode) o;
			CTerm term = t.term;
			if(term instanceof CVariable){
				return ((CVariable)term).getVariableName();
			}
			return t.term.getFunctorValue()+"/"+t.term.getArity();
		}
		
		return o.toString();
	}

	
	
	

	public Object getParent(Object o) {
		if (o instanceof Predicate) {
			Predicate p = (Predicate) o;
			return p.getModule();
		} else if (o instanceof Clause) {
			Clause c = (Clause) o;
			return c.getPredicate();
		}
		return null;
	}

	public Image getImage(Object object) {

		if (object instanceof String) {// FIXME: need a module type --lu
			return ImageRepository.getImage(ImageRepository.PE_MODULE);
		} else if (object instanceof Predicate) {
			Predicate p = (Predicate) object;
			boolean exported = p.isPublic();
			return ImageRepository
					.getImage(exported ? ImageRepository.PE_PUBLIC
							: ImageRepository.PE_HIDDEN);
		}
		if (object instanceof Clause) {
			return ImageRepository.getImage(ImageRepository.PE_CLAUSE);
		}
		if (object instanceof Directive) {
			return ImageRepository.getImage(ImageRepository.PE_CLAUSE);
		}

		String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
		return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);

	}

	public String getText(Object element) {
		return getLabel(element);
	}

	public void addListener(ILabelProviderListener listener) {
	}

	public void dispose() {

	}

	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
	}



	
	
}

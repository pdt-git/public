/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.internal;


import org.cs3.pdt.common.metadata.Predicate;
import org.cs3.pdt.common.structureElements.PrologTreeElement;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;

public class PrologTreeElementLabelDecorator extends LabelProvider implements ILightweightLabelDecorator{
	

	@Override
	public void decorate(Object element, IDecoration decoration) {
		if(!(element instanceof PrologTreeElement)){
			return;
		}
		if (element instanceof Predicate) {
			Predicate predicate = (Predicate)element;
			if (predicate.isMultifile())
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PE_MULTIFILE));
		}
	}
	
	
	@Override
	public void addListener(ILabelProviderListener listener) {
		}

	@Override
	public void dispose() {
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {

	}

}



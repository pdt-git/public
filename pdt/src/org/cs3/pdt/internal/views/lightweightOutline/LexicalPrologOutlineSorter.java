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

package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.structureElements.OutlineClauseElement;
import org.cs3.pdt.internal.structureElements.OutlineFileElement;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

final class LexicalPrologOutlineSorter extends ViewerSorter {

	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
		if ((e1 instanceof OutlineClauseElement) && (e2 instanceof OutlineFileElement)) {
			return -1;
		} else if ((e1 instanceof OutlineFileElement) && (e2 instanceof OutlineClauseElement)) {
			return 1;
		} else if ((e1 instanceof OutlineFileElement) && (e2 instanceof OutlineFileElement)) {
			return ((OutlineFileElement) e1).compareTo((OutlineFileElement) e2);
		} else if ((e1 instanceof OutlineClauseElement) && (e2 instanceof OutlineClauseElement)) {
			OutlineClauseElement occ1 = (OutlineClauseElement)e1;
			OutlineClauseElement occ2 = (OutlineClauseElement)e2;
			if (occ1.getType().equals(occ2.getType()))
				return (occ1).getLine() - (occ2).getLine();
			if ((occ1.getType().equals("declaration")) || (occ2.getType().equals("multifile")))
				return -1;
			if ((occ1.getType().equals("multifile")) || (occ1.getType().equals("declaration")))
				return 1;
		}
		return super.compare(viewer, e1, e2);
	}
}


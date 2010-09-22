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

/*
 * Created on 20.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal.editors;


import org.cs3.pl.common.Debug;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultAutoIndentStrategy;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextUtilities;
/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PLAutoIndentStrategy extends DefaultAutoIndentStrategy implements IAutoEditStrategy{

	private void autoIndentAfterNewLine(IDocument d, DocumentCommand c) {
		
		if (c.offset == -1 || d.getLength() == 0)
			return;
		
		try {
			// find start of line
			int p= (c.offset == d.getLength() ? c.offset  - 1 : c.offset);
			IRegion info= d.getLineInformationOfOffset(p);
			
			int start= info.getOffset();
					
			if ((c.offset == start + info.getLength()) && isPredicateHead(d,info)) 
				predicateIndent(d,c);
			
			// find white spaces
			int end= findEndOfWhiteSpace(d, start, c.offset);
			
			StringBuffer buf= new StringBuffer(c.text);
			if (end > start) {			
				// append to input
				buf.append(d.get(start, end - start));
			}
			
			c.text= buf.toString();
			
		} catch (BadLocationException excp) {
			// stop work
		}	
	}
	
	/**
	 * 
	 */
	private void predicateIndent(IDocument d,DocumentCommand c) {
		String nl = TextUtilities.getDefaultLineDelimiter(d);
		c.text=nl+"    ";
		
	}

	/**
	 * @param info
	 * @return
	 */
	private boolean isPredicateHead(IDocument d, IRegion info) {
		String line;
		try {
			line = d.get(info.getOffset(), info.getLength());
			String removedWS =line.replaceAll(" ", "");
			return removedWS.endsWith(":-");
		} catch (BadLocationException e) {
			Debug.report(e);
		}
		return false;
	}

	/*
	 * @see IAutoIndentStrategy#customizeDocumentCommand
	 */
	@Override
	public void customizeDocumentCommand(IDocument d, DocumentCommand c) {
		if (c.length == 0 && c.text != null && TextUtilities.endsWith(d.getLegalLineDelimiters(), c.text) != -1)
			autoIndentAfterNewLine(d, c);
	}
	
}

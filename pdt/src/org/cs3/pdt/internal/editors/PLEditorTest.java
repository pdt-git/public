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
 * Created on 02.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal.editors;

import junit.framework.TestCase;

import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.GoalDataProvider;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;

/**
 * @author windeln
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PLEditorTest extends TestCase {
	public void testGetPrologDataFromOffset() throws BadLocationException {
		Document document = new Document("  , ahaha(a,b,c).");
		Goal data = GoalDataProvider.getPrologDataFromOffset(null,document, 6);
		assertEquals("ahaha",data.getName());
		assertEquals(3,data.getArity());

		document = new Document("  , ahah1a(a,b(a),c).");
		data = GoalDataProvider.getPrologDataFromOffset(null,document, 6);
		assertEquals("ahah1a",data.getName());
		assertEquals(3,data.getArity());

		document = new Document("  , ahaha(\"asdf\\\"\'\",b(a),c).");
		data = GoalDataProvider.getPrologDataFromOffset(null,document, 6);
		assertEquals("ahaha",data.getName());
		assertEquals(3,data.getArity());
	
		document = new Document("  , ahaha(\"as,df\\\"\'\",b(a),[c,b]).");
		data = GoalDataProvider.getPrologDataFromOffset(null,document, 6);
		assertEquals("ahaha",data.getName());
		assertEquals(3,data.getArity());

		document = new Document("  , aha_ha(\"as,df\\\"\'\",b(a),[c,b]).");
		data = GoalDataProvider.getPrologDataFromOffset(null,document, 6);
		assertEquals("aha_ha",data.getName());
		assertEquals(3,data.getArity());

		document = new Document(" test/12");
		data = GoalDataProvider.getPrologDataFromOffset(null,document, 3);
		assertEquals("test",data.getName());
		assertEquals(12,data.getArity());

		document = new Document("type: ");
		data = GoalDataProvider.getPrologDataFromOffset(null,document, 3);
		assertEquals("type",data.getName());
		assertEquals(-1,data.getArity());
	}
}

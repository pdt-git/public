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

package org.cs3.pdt.console.internal.views;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;

public class PrologCompletionProvider {

	private static final CompletionProposal[] EMPTY_COMPLETION_PROPOSAL = new CompletionProposal[0];

	private PrologInterface pif;

	public CompletionProposal[] getCompletionProposals(String line, int pos) {
		String head = line.substring(0, pos);

		String[] split = head.split("[^\\w^$]");
		if (split.length == 0) {
			return EMPTY_COMPLETION_PROPOSAL;
		}
		String prefix = split[split.length - 1];
		int prefixLength = prefix.length();
		
		if (prefixLength <= 0) {
			return EMPTY_COMPLETION_PROPOSAL;
		}
		
		ArrayList<CompletionProposal> proposals = new ArrayList<CompletionProposal>();
		String query = bT("pdt_search:find_pred", "'_'", Util.quoteAtom(prefix), "_", "Name", "Arity", "Public", "_" , "_");
		List<Map<String, Object>> results;
		try {
			results = pif.queryAll(query);
			for (Map<String,Object> result : results) {
				int arity = Integer.parseInt(result.get("Arity").toString());
				String name = result.get("Name").toString();
				proposals.add(new CompletionProposal(name, arity, prefixLength));
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		}
		return proposals.toArray(new CompletionProposal[proposals.size()]);
	}
	
	public void setPrologInterface(PrologInterface pif) {
		this.pif = pif;
	}

	public PrologInterface getPrologInterface() {
		return pif;
	}
}
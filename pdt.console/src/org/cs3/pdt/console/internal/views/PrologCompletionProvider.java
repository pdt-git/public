/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.console.internal.views;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonPredicates;
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
		String query = bT(PDTCommonPredicates.FIND_PRED, "'_'", Util.quoteAtom(prefix), "_", "Name", "Arity", "Public", "_" , "Doc");
		List<Map<String, Object>> results;
		try {
			results = pif.queryAll(query);
			for (Map<String,Object> result : results) {
				int arity = Integer.parseInt(result.get("Arity").toString());
				String name = result.get("Name").toString();
				String doc = result.get("Doc").toString();
				proposals.add(new CompletionProposal(name, arity, prefixLength, doc));
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



/* $LICENSE_MSG$(ld) */

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


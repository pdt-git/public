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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;

public class PrologCompletionProvider {

	private static final CompletionProposal[] EMPTY_COMPLETION_PROPOSAL = new CompletionProposal[0];

	private class _Result implements CompletionResult {

		@Override
		public String getOriginalLineContent() {
			return line;
		}

		@Override
		public int getOriginalCaretPosition() {
			return pos;
		}

		@Override
		public String[] getOptions() {
			if (options == null) {
				return null;
			}
			String[] result = new String[options.size()];
			int i = 0;
			for (Iterator<String> it = options.iterator(); it.hasNext(); i++) {
				String o = it.next();
				result[i] = o;
			}
			return result;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.cs3.pl.views.ConsoleCompletionProvider#getCaretPosition()
		 */
		@Override
		public int getNewCaretPosition() {
			return newPos;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.cs3.pl.views.ConsoleCompletionProvider#getNewLineContent()
		 */
		@Override
		public String getNewLineContent() {
			return newLine;
		}

		String line = null;

		String newLine = null;

		int newPos = -1;

		TreeSet<String> options = null;

		int pos = -1;

	}

	TreeSet<Completion> completions = null;

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
	
	/**
	 * complete the line.
	 * 
	 * @param line
	 *            the content of the line buffer
	 * @param pos
	 *            the carret position
	 * @return the completed line. If there is no  completeion, the line
	 *         should be returned unchanged.
	 * 			If there is more than one option, the provider should
	 * 			try to do as much as possible.
	 */
	public CompletionResult doCompletion(String line, int pos) {
		if (pif == null) {
			return null;
		}
		
		_Result r = new _Result();
		r.line = line;
		r.pos = pos;
		String head = line.substring(0, pos);
		String tail = line.substring(pos);

		String[] split = head.split("[^\\w^$]");
		String prefix = split[split.length - 1];

		try {
			r.options = new TreeSet<String>();
			completions = new TreeSet<Completion>();
			
			findPredicatesWithPrefix(null, prefix, null, r.options, completions);
		} catch (NumberFormatException e) {
			Debug.report(e);
		} catch (PrologException e) {
			Debug.report(e);
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		}

		Completion completion = completions == null || completions.isEmpty() ? new Completion("",0)
				: completions.first();
		if (completions == null || completions.size() == 0) {
			r.newLine = line;
			r.newPos = pos;
		} else if (completions.size() == 1) {
			
			if (head.equals(completion.getFunctor() + "(")) {
				r.newLine = completion.getFunctor() + completion.getArglist() + tail;
				r.newPos = pos - prefix.length() + completion.getFunctor().length() - 1;
			} else if (head.startsWith(completion.getFunctor()) && !head.equals(completion.getFunctor())) {
				r.newLine = head;
				r.newPos = pos;
			} else {
				r.newLine = head + completion.getFunctor().substring(prefix.length()) + completion.getArglist() + tail;
				r.newPos = pos - prefix.length() + completion.getFunctor().length();
				if(completion.getArity() > 0) {
					r.newPos++;
				}
			}
			
		} else {
			int commonLength = getCommonLength();
			String commonPart = completion.getFunctor().substring(prefix.length(),
					commonLength);
			r.newLine = head + commonPart + tail;
			r.newPos = pos - prefix.length() + commonLength;
		}
		return r;
	}

	public void findPredicatesWithPrefix(String module, String prefix, String filename, TreeSet<String> signatures, TreeSet<Completion> functors) throws NumberFormatException, PrologException,
			PrologInterfaceException {

		PrologSession session = pif.getSession(PrologInterface.NONE);
		try {
			if (module == null)
				module = "_";
			if (filename == null)
				filename = "_";
			String query = "pdt_search:find_pred('" + filename + "','" + prefix + "', "
					+ module + ",Name,Arity,Public,_,_)";
			List<Map<String,Object>> results = session.queryAll(query);
			for (Map<String,Object> result : results) {
				int arity = Integer.parseInt(result.get("Arity").toString());
				String name = result.get("Name").toString();
				signatures.add(name + "/" + arity);
				functors.add(new Completion(name,arity));
			}
		} finally {
			if (session != null) {
				session.dispose();
			}
		}

	}

	// propably there is a smarter way of doing this...
	int getCommonLength() {
		int len = 1;
		while (true) {

			String first = Util.unquoteAtom(completions.first().getFunctor());
			String last = Util.unquoteAtom(completions.last().getFunctor());
			if (first.length() < len || last.length() < len) {
				break;
			}
			String a = first.substring(0, len);
			String b = last.substring(0, len);
			if (!a.equals(b)) {
				break;
			}
			len++;
		}
		return len - 1;
	}

	public void setPrologInterface(PrologInterface pif) {
		this.pif = pif;
	}

	public PrologInterface getPrologInterface() {
		return pif;
	}
}
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

package org.cs3.pdt.internal.contentassistant;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.swt.graphics.Image;

public abstract class NaivPrologContentAssistProcessor extends PrologContentAssistProcessor implements IContentAssistProcessor {

	@Override
	protected void addVariableProposals(IDocument document, int begin, int len, String prefix, List<ComparableCompletionProposal> proposals) throws BadLocationException {

		Set<String> unique = new HashSet<String>();
		Image image = ImageRepository.getImage(ImageRepository.PE_PUBLIC);

		if (Util.isVarPrefix(prefix) || prefix.length() == 0) {
			int l = begin == 0 ? begin : begin - 1;
			String proposal = null;
			while (l > 0 && !PLEditor.predicateDelimiter(document, l)) {
				ITypedRegion region = document.getPartition(l);
				if (isComment(region))
					l = region.getOffset();
				else {
					char c = document.getChar(l);
					if (Util.isVarChar(c)) {
						if (proposal == null)
							proposal = "";
						proposal = c + proposal;
					} else if (proposal != null) {
						if (Util.isVarPrefix(proposal.charAt(0)) && proposal.regionMatches(true, 0, prefix, 0, prefix.length()) && !unique.contains(proposal) /*
																																							 * &&
																																							 * !
																																							 * proposal
																																							 * .
																																							 * equals
																																							 * (
																																							 * "_"
																																							 * )
																																							 */) {
							unique.add(proposal);
							int cursorPos = proposal.length();
							proposals.add(new VariableCompletionProposal(proposal, begin, len, cursorPos, image, proposal, null, null));
						}
						proposal = null;
					}
				}
				l--;
			}
		}
		if (Util.isVarPrefix(prefix) || prefix.length() == 0) {
			int l = begin == document.getLength() ? begin : begin + 1;
			String proposal = null;
			while (l < document.getLength() && !PLEditor.predicateDelimiter(document, l)) {
				ITypedRegion region = document.getPartition(l);
				if (isComment(region)) {
					l = region.getOffset() + region.getLength();
				} else {
					char c = document.getChar(l);
					if (Util.isVarChar(c)) {
						if (proposal == null)
							proposal = "";
						proposal = proposal + c;
					} else if (proposal != null) {
						if (Util.isVarPrefix(proposal.charAt(0)) && proposal.regionMatches(true, 0, prefix, 0, prefix.length()) && !unique.contains(proposal) /*&& !proposal.equals("_")*/) {
							unique.add(proposal);
							int cursorPos = proposal.length();
							proposals.add(new VariableCompletionProposal(proposal, begin, len, cursorPos, image, proposal, null, null));
						}
						proposal = null;
					}
				}
				l++;
			}
		}
	}

	@Override
	protected void addPredicateProposals(IDocument document, int begin, int len, String prefix, List<ComparableCompletionProposal> proposals, String module) throws PrologInterfaceException,
			CoreException {

		if (Util.isVarPrefix(prefix)) {
			return;
		}
		PrologSession session = null;
		try {
			String enclFile = UIUtils.getFileFromActiveEditor();
			String moduleArg = module != null ? Util.quoteAtom(module) : "Module";
			session = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface().getSession();
			String query = bT(PDTCommonPredicates.FIND_PRED_FOR_EDITOR_COMPLETION,
					Util.quoteAtom(enclFile),
					Util.quoteAtom(prefix),
					moduleArg,
					"Name",
					"Arity",
					"Public",
					"Builtin",
					"Doc",
					"Kind");
			List<Map<String, Object>> predicates = session.queryAll(query);
			Debug.info("find predicates with prefix: " + query);
			for (Map<String, Object> predicate : predicates) {
				String name = (String) predicate.get("Name");
				String strArity = (String) predicate.get("Arity");
				String kind = predicate.get("Kind").toString();
				if (predicate.get("Module") != null) {
					module = (String) predicate.get("Module");
					if (module.equals("_")) {
						module = null;
					}
				}

				int arity = Integer.parseInt(strArity);
				String doc = (String) predicate.get("Doc");
				Map<String, String> tags = new HashMap<String, String>();
				if (Boolean.parseBoolean((String) predicate.get("Public"))) {
					tags.put("public", "true");
				}
				if (Boolean.parseBoolean((String) predicate.get("Builtin"))) {
					tags.put("built_in", "true");
				}

				if (!doc.equals("nodoc")) {
					if (doc.startsWith("%%")) {

						doc = doc.replaceAll("%", "").trim();
					}

					tags.put("documentation", doc);
				}

				ComparableCompletionProposal p = new PredicateCompletionProposal(document, begin, len, name, arity, tags, module, kind);
				proposals.add(p);
			}
			return;
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null)
				session.dispose();
		}
		return;

	}

}



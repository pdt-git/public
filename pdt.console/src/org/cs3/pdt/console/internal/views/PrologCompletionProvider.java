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
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.jface.fieldassist.IContentProposal;

public class PrologCompletionProvider {

	private static final PredicateCompletionProposal[] EMPTY_COMPLETION_PROPOSAL = new PredicateCompletionProposal[0];

	private PrologInterface pif;

	@SuppressWarnings("unchecked")
	public IContentProposal[] getCompletionProposals(String line, int pos) {
//		String head = line.substring(0, pos);
//
//		String[] split = head.split("[^\\w^$]");
//		if (split.length == 0) {
//			return EMPTY_COMPLETION_PROPOSAL;
//		}
//		String prefix = split[split.length - 1];
		if (line.isEmpty()) {
			return EMPTY_COMPLETION_PROPOSAL;
		}
		
		String searchPrefix;
		Prefix prefix = calculatePrefix(line, pos - 1);
		String splittingOperator = findSplittingOperator(line, prefix.begin - 1);
		String module = null;
		if (splittingOperator != null) {
			module = retrievePrefixedModule(line, prefix.begin - splittingOperator.length());
		}
		if (module == null || module.isEmpty()) {
			searchPrefix = Util.quoteAtomIfNeeded(prefix.prefix);
		} else {
			if (Util.isVarPrefix(module)){
				module = "_";
			} else {
				module = Util.quoteAtomIfNeeded(module);
			}
			searchPrefix = module + splittingOperator + Util.quoteAtomIfNeeded(prefix.prefix);
		}
		
		if (prefix.length <= 0) {
			return EMPTY_COMPLETION_PROPOSAL;
		}
		
		ArrayList<IContentProposal> proposals = new ArrayList<IContentProposal>();
		String query = bT(PDTCommonPredicates.FIND_COMPLETION,
				"_",
				"_",
				searchPrefix,
				"Kind",
				"Module",
				"Name",
				"Arity",
				"Visibility",
				"Builtin",
				"ArgNames",
				"DocKind",
				"Doc");
		List<Map<String, Object>> results;
		try {
			results = pif.queryAll(query);
			for (Map<String,Object> result : results) {
				String kind = result.get("Kind").toString();
				String name = result.get("Name").toString();
				if (SearchConstants.COMPLETION_KIND_PREDICATE.equals(kind)) {
					int arity = Integer.parseInt(result.get("Arity").toString());
					String visibility = result.get("Visibility").toString();
					boolean isBuiltin = Boolean.parseBoolean(result.get("Builtin").toString());
					Object argNamesValue = result.get("ArgNames");
					List<String> argNames = null;
					if (argNamesValue instanceof List<?>) {
						argNames = (List<String>) argNamesValue;
					}
					proposals.add(new PredicateCompletionProposal(name, arity, prefix.length, visibility, isBuiltin, argNames, prefix.startsWithSingleQuote));
				} else if (SearchConstants.COMPLETION_KIND_MODULE.equals(kind)){
					proposals.add(new ModuleCompletionProposal(name, prefix.length, prefix.startsWithSingleQuote));
				} else if (SearchConstants.COMPLETION_KIND_ATOM.equals(kind)){
					proposals.add(new AtomCompletionProposal(name, prefix.length, prefix.startsWithSingleQuote));
				}
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		}
		return proposals.toArray(new IContentProposal[proposals.size()]);
	}
	
	private class Prefix {
		int begin;
		int length;
		String prefix;
		boolean startsWithSingleQuote;
				
		Prefix(int begin, String prefix, boolean startsWithSingleQuote) {
			this.begin = begin;
			this.prefix = prefix;
			this.length = prefix.length();
			this.startsWithSingleQuote = startsWithSingleQuote;
		}
	}
		
	private Prefix calculatePrefix(String line, int offset) {
		int begin=offset;
		int length=0;
		char c = line.charAt(begin);
		if (c == '\'') {
			return new Prefix(offset, "", false);
		}
		boolean isPredChar = Util.isNonQualifiedPredicateNameChar(c);
		
		while (isPredChar){
			length++;
			int test = begin-1;
			if(test >=0){
				c = line.charAt(test);
				if (c == '\'') {
					return new Prefix(begin - 1, line.substring(begin, begin + length), true);
				}
				isPredChar = Util.isNonQualifiedPredicateNameChar(c);
				if(!isPredChar){
					break;
				}
			} else {
				break;
			}
			begin=test;
		}
		return new Prefix(begin, line.substring(begin, begin + length), false);
	}
	
	private String findSplittingOperator(String line, int begin) {
		if (begin <= 0) {
			return null;
		}
		char c = line.charAt(begin);
		char c2 = line.charAt(begin - 1);
		switch (c) {
		case ':':
			switch (c2) {
			case ':':
				return "::";
			default:
				return ":";
			}
		case '<':
			if (c2 == '<') {
				return "<<";
			} else {
				return null;
			}
		}
		return null;
	}
	
	private String retrievePrefixedModule(String line, int begin) {
		int moduleEnd = begin;
		int moduleBegin = begin - 1;
		while (moduleBegin >= 0 && Util.isNonQualifiedPredicateNameChar(line.charAt(moduleBegin)))
			moduleBegin--;
		String moduleName = line.substring(moduleBegin + 1, moduleEnd);
//		if(!Util.isVarPrefix(moduleName)){
			return moduleName;
//		} else {
//			return "_";
//		}
	}
	public void setPrologInterface(PrologInterface pif) {
		this.pif = pif;
	}

	public PrologInterface getPrologInterface() {
		return pif;
	}
}



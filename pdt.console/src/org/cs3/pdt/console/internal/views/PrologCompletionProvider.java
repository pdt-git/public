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
		
		Prefix prefix = calculatePrefix(line, pos - 1);
		String splittingOperator = findSplittingOperator(line, prefix.begin - 1);
		String module = null;
		if (splittingOperator != null) {
			module = retrievePrefixedModule(line, prefix.begin - splittingOperator.length());
		}
		if (module == null || module.isEmpty()) {
			module = "_";
		} else {
			module = Util.quoteAtomIfNeeded(module);
		}
		
		if (prefix.length <= 0) {
			return EMPTY_COMPLETION_PROPOSAL;
		}
		
		ArrayList<CompletionProposal> proposals = new ArrayList<CompletionProposal>();
		String query = bT(PDTCommonPredicates.FIND_PRED, "'_'", Util.quoteAtomIfNeeded(prefix.prefix), module, "Name", "Arity", "Public", "_" , "Doc");
		List<Map<String, Object>> results;
		try {
			results = pif.queryAll(query);
			for (Map<String,Object> result : results) {
				int arity = Integer.parseInt(result.get("Arity").toString());
				String name = result.get("Name").toString();
				String doc = result.get("Doc").toString();
				proposals.add(new CompletionProposal(name, arity, prefix.length, doc, prefix.startsWithSingleQuote));
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		}
		return proposals.toArray(new CompletionProposal[proposals.size()]);
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
			}
		}
		return null;
	}
	
	private String retrievePrefixedModule(String line, int begin) {
		int moduleEnd = begin;
		int moduleBegin = begin - 1;
		while (Util.isNonQualifiedPredicateNameChar(line.charAt(moduleBegin)) && moduleBegin > 0)
			moduleBegin--;
		String moduleName = line.substring(moduleBegin, moduleEnd);
		if(!Util.isVarPrefix(moduleName)){
			return moduleName;
		} else {
			return null;
		}
	}
	public void setPrologInterface(PrologInterface pif) {
		this.pif = pif;
	}

	public PrologInterface getPrologInterface() {
		return pif;
	}
}



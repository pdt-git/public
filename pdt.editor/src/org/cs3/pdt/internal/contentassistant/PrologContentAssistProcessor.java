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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.cs3.pdt.internal.editors.PLPartitionScanner;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;

public abstract class PrologContentAssistProcessor {

	public PrologContentAssistProcessor() {
		super();
	}

	protected abstract IFile getFile() throws CoreException;
	
	private class Prefix {
		int begin;
		int length;
		String prefix;
				
		Prefix(IDocument document, int begin, String prefix) {
			this.begin=begin;
			this.prefix=prefix;
			this.length=prefix.length();
		}
	}
		
	protected abstract void addPredicateProposals(IDocument document, int begin,
			int len, String prefix, String searchPrefixForDefault, List<ComparableTemplateCompletionProposal> proposals)
			throws PrologInterfaceException, CoreException;

	protected abstract void addVariableProposals(IDocument document, int begin,
			int len, String prefix, List<ComparableTemplateCompletionProposal> proposals) throws BadLocationException, PrologInterfaceException, CoreException;

	private Prefix calculatePrefix(IDocument document, int offset)
			throws BadLocationException {
		int begin=offset;
		int length=0;
		boolean isPredChar = Util.isNonQualifiedPredicateNameChar(document.getChar(begin));
		
		while (isPredChar){
			length++;
			int test = begin-1;
			if(test >=0){
				isPredChar = Util.isNonQualifiedPredicateNameChar(document.getChar(test));
				if(!isPredChar){
					break;
				}
			} else {
				break;
			}
			begin=test;
		}
		String pre = document.get(begin, length);
		
		Prefix prefix = new Prefix(document,begin,pre);
		return prefix;
	}

	private String findSplittingOperator(IDocument document, int begin) throws BadLocationException {
		if (begin <= 0) {
			return null;
		}
		char c = document.getChar(begin);
		char c2 = document.getChar(begin - 1);
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
		case '^':
			if (c2 == '^') {
				return "^^";
			}
		}
		return null;
	}
	
	private String retrievePrefixedModule(IDocument document, int begin)
			throws BadLocationException {
		int moduleEnd = begin;
		int moduleBegin = begin - 1;
		while (moduleBegin >= 0 && Util.isNonQualifiedPredicateNameChar(document.getChar(moduleBegin)))
			moduleBegin--;
		String moduleName = document.get(moduleBegin + 1, moduleEnd - moduleBegin - 1);
		return moduleName;
	}

	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
	
		try {
			IDocument document = viewer.getDocument();
	
			documentOffset = documentOffset == 0 ? documentOffset
					: documentOffset - 1;
	
			Prefix pre = calculatePrefix(document,documentOffset);
			
			String splittingOperator = findSplittingOperator(document, pre.begin - 1);
	
			String module = null;
			if (splittingOperator != null) {
				module = retrievePrefixedModule(document, pre.begin - splittingOperator.length());
			}
			String searchPrefix;
			String searchPrefixForDefault = null;
			
			ArrayList<ComparableTemplateCompletionProposal> proposals = new ArrayList<ComparableTemplateCompletionProposal>();
			if (module == null || module.equals("")) {
				if (pre.prefix.equals("")) {
					return null;
				}
				addVariableProposals(document, pre.begin, pre.length, pre.prefix, proposals);
				if (splittingOperator != null) {
					searchPrefix = splittingOperator + Util.quoteAtomIfNeeded(pre.prefix);
				} else {
					searchPrefix = Util.quoteAtomIfNeeded(pre.prefix);
					searchPrefixForDefault = pre.prefix;
				}
			} else {
				if (Util.isVarPrefix(module)){
					module = "_";
				} else {
					module = Util.quoteAtomIfNeeded(module);
				}
				searchPrefix = module + splittingOperator + Util.quoteAtomIfNeeded(pre.prefix);
			}
			if (!Util.isVarPrefix(pre.prefix)) {
				addPredicateProposals(document, pre.begin, pre.length, searchPrefix, searchPrefixForDefault, proposals);
			}
	
			if (proposals.size() == 0)
				return null;
			Collections.sort(proposals);
			return proposals
					.toArray(new ICompletionProposal[proposals.size()]);
		} catch (BadLocationException e) {
			Debug.report(e);
//			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
//					.getErrorMessageProvider(), viewer.getTextWidget()
//					.getShell(), PDT.ERR_COMPLETION_BAD_LOCATION,
//					PDT.CX_COMPLETION, e);
			return null;
		} catch (PrologInterfaceException e) {
			Debug.report(e);
//			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
//					.getErrorMessageProvider(), viewer.getTextWidget()
//					.getShell(), PDT.ERR_PIF, PDT.CX_COMPLETION, e);
			return null;
		} catch (CoreException e) {
			Debug.report(e);
//			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
//					.getErrorMessageProvider(), viewer.getTextWidget()
//					.getShell(), PDT.ERR_CORE_EXCEPTION, PDT.CX_COMPLETION, e);
			return null;
		} finally {
	
		}
	}

	protected boolean isComment(ITypedRegion region) {
		return region.getType().equals(PLPartitionScanner.PL_COMMENT)
				|| region.getType().equals(PLPartitionScanner.PL_MULTI_COMMENT);
	}

	public IContextInformation[] computeContextInformation(ITextViewer viewer, int offset) {
	
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
	
		return new char[0];
	}

	public char[] getContextInformationAutoActivationCharacters() {
		return new char[0];
	}

	public IContextInformationValidator getContextInformationValidator() {
		class Validator implements IContextInformationValidator {
	
			@Override
			public boolean isContextInformationValid(int position) {
				return true;
			}
	
			@Override
			public void install(IContextInformation info, ITextViewer viewer,
					int documentPosition) {
				;
	
			}
		}
		return new Validator();
	}

	public String getErrorMessage() {
		return "Error Message?";
	}

}



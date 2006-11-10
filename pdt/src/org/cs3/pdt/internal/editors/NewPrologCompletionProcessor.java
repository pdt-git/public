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

package org.cs3.pdt.internal.editors;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.views.PredicateNode;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.PrologSession2;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IFileEditorInput;

public class NewPrologCompletionProcessor implements IContentAssistProcessor {

	private String errorMsg;

	/**
	 * @param documentOffset
	 * @param document
	 * @param begin
	 * @param module
	 * @return
	 * @throws BadLocationException
	 */
	private String retrievePrefixedModule(int documentOffset,
			IDocument document, int begin) throws BadLocationException {
		if (document.getChar(begin - 1) == ':') {
			int moduleBegin = begin - 2;
			while (PLEditor.isNonQualifiedPredicatenameChar(document
					.getChar(moduleBegin))
					&& moduleBegin > 0)
				moduleBegin--;
			return document.get(moduleBegin + 1, documentOffset - moduleBegin);
		}
		return null;
	}

	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer,
			int documentOffset) {

		try {
			IDocument document = viewer.getDocument();

			documentOffset = documentOffset == 0 ? documentOffset
					: documentOffset - 1;
			int begin = documentOffset;

			while (PLEditor.isNonQualifiedPredicatenameChar(document
					.getChar(begin))
					&& begin > 0)
				begin--;
			int len = documentOffset - begin;
			begin++;
			String prefix = document.get(begin, len);

			String module = retrievePrefixedModule(documentOffset - len - 1,
					document, begin);

			List proposals = new ArrayList();
			if (module == null || module.equals("")) {
				if (prefix.equals("")) {
					return null;
				}
				addVariableProposals(document, begin, len, prefix, proposals);
			}
			addPredicateProposals(document, begin, len, prefix, proposals,
					module);

			if (proposals.size() == 0)
				return null;
			return (ICompletionProposal[]) proposals
					.toArray(new ICompletionProposal[proposals.size()]);
		} catch (BadLocationException e) {
			Debug.report(e);
			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
					.getErrorMessageProvider(), viewer.getTextWidget()
					.getShell(), PDT.ERR_COMPLETION_BAD_LOCATION,
					PDT.CX_COMPLETION, e);
			return null;
		} catch (PrologInterfaceException e) {
			Debug.report(e);
			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
					.getErrorMessageProvider(), viewer.getTextWidget()
					.getShell(), PDT.ERR_PIF, PDT.CX_COMPLETION, e);
			return null;
		} catch (CoreException e) {
			Debug.report(e);
			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
					.getErrorMessageProvider(), viewer.getTextWidget()
					.getShell(), PDT.ERR_CORE_EXCEPTION, PDT.CX_COMPLETION, e);
			return null;
		} finally {

		}
	}

	private void addVariableProposals(IDocument document, int begin, int len,
			String prefix, List proposals) throws BadLocationException {

		Set unique = new HashSet();
		Image image = ImageRepository.getImage(ImageRepository.PE_PUBLIC);

		if (PLEditor.isVarPrefix(prefix) || prefix.length() == 0) {
			int l = begin == 0 ? begin : begin - 1;
			String proposal = null;
			while (l > 0 && !PLEditor.predicateDelimiter(document, l)) {
				ITypedRegion region = document.getPartition(l);
				if (isComment(region))
					l = region.getOffset();
				else {
					char c = document.getChar(l);
					if (PLEditor.isVarChar(c)) {
						if (proposal == null)
							proposal = "";
						proposal = c + proposal;
					} else if (proposal != null) {
						if (PLEditor.isVarPrefix(proposal.charAt(0))
								&& proposal.regionMatches(true, 0, prefix, 0,
										prefix.length())
								&& !unique.contains(proposal) /*
																 * &&
																 * !proposal.equals("_")
																 */) {
							unique.add(proposal);
							int cursorPos = proposal.length();
							proposals.add(new CompletionProposal(proposal,
									begin, len, cursorPos, image, proposal,
									null, null));
						}
						proposal = null;
					}
				}
				l--;
			}
		}
		if (PLEditor.isVarPrefix(prefix) || prefix.length() == 0) {
			int l = begin == document.getLength() ? begin : begin + 1;
			String proposal = null;
			while (l < document.getLength()
					&& !PLEditor.predicateDelimiter(document, l)) {
				ITypedRegion region = document.getPartition(l);
				if (isComment(region)) {
					l = region.getOffset() + region.getLength();
				} else {
					char c = document.getChar(l);
					if (PLEditor.isVarChar(c)) {
						if (proposal == null)
							proposal = "";
						proposal = proposal + c;
					} else if (proposal != null) {
						if (PLEditor.isVarPrefix(proposal.charAt(0))
								&& proposal.regionMatches(true, 0, prefix, 0,
										prefix.length())
								&& !unique.contains(proposal) /*
																 * &&
																 * !proposal.equals("_")
																 */) {
							unique.add(proposal);
							int cursorPos = proposal.length();
							proposals.add(new CompletionProposal(proposal,
									begin, len, cursorPos, image, proposal,
									null, null));
						}
						proposal = null;
					}
				}
				l++;
			}
		}
	}

	private boolean isComment(ITypedRegion region) {
		return region.getType().equals(PLPartitionScanner.PL_COMMENT)
				|| region.getType().equals(PLPartitionScanner.PL_MULTI_COMMENT);
	}

	private List addProposals(int begin, int len, String prefix,
			List proposals, Predicate[] elems) throws CoreException {
		IFileEditorInput editorInput = (IFileEditorInput) UIUtils
				.getActiveEditor().getEditorInput();

		String activeFileName = Util.prologFileName(editorInput.getFile()
				.getLocation().toFile());
		IPrologProject plProject;
		plProject = (IPrologProject) editorInput.getFile().getProject()
				.getNature(PDTCore.NATURE_ID);

		PrologInterface pif = plProject.getMetadataPrologInterface();

		for (int i = 0; i < elems.length; i++) {
			ICompletionProposal proposal = new NewPrologCompletionProposal(pif,
					elems[i], begin, len, prefix);
			proposals.add(proposal);

		}
		return proposals;
	}

	private void addPredicateProposals(IDocument document, int begin, int len,
			String prefix, List proposals, String module)
			throws PrologInterfaceException, CoreException {

		if (PLEditor.isVarPrefix(prefix)) {
			return;
		}
		Predicate[] elems = null;

		elems = getPredicatesWithPrefix(module, prefix);

		if (elems == null) {
			elems = new Predicate[0];
		}
		addProposals(begin, len, prefix, proposals, elems);
	}

	private Predicate[] getPredicatesWithPrefix(String contextModule,
			String prefix) throws PrologInterfaceException, CoreException {

		IFileEditorInput editorInput = (IFileEditorInput) UIUtils
				.getActiveEditor().getEditorInput();

		String activeFileName = Util.prologFileName(editorInput.getFile()
				.getLocation().toFile());
		IPrologProject plProject;
		plProject = (IPrologProject) editorInput.getFile().getProject()
				.getNature(PDTCore.NATURE_ID);

		PrologSession2 session = (PrologSession2) plProject.getMetadataPrologInterface()
				.getSession();
		
		
		session.setPreferenceValue("socketsession.interprete_lists", "false");
		Predicate[] elms = null;
		try {
			String query = null;
			if (contextModule == null) {
				query = "pdt_file_module('" + activeFileName
						+ "',Context),pdt_predicate_completion(Context,'"
						+ prefix + "',Name,[module(Module)," + "arity(Arity),"
						+ "exported(Exported)," + "dynamic(Dynamic),"
						+ "multifile(Multifile),"
						+ "transparent(Transparent)]),"
						+ "pdt_help_summary(Module:Name/Arity,_,_,Summary)";
			} else {
				query = "pdt_predicate_completion(" + contextModule + ",'"
						+ prefix + "',Name,[module(Module)," + "arity(Arity),"
						+ "exported(Exported)," + "dynamic(Dynamic),"
						+ "multifile(Multifile),"
						+ "transparent(Transparent)]),"
						+ "pdt_help_summary(Module:Name/Arity,_,_,Summary)";
			}

			List l = session.queryAll(query);
			elms = new Predicate[l.size()];
			int i = 0;
			for (Iterator iter = l.iterator(); iter.hasNext();) {
				Map m = (Map) iter.next();
				String module = (String) m.get("Module");
				String name = (String) m.get("Name");
				int arity = Integer.parseInt((String) m.get("Arity"));
				PredicateNode p = new PredicateNode(module, name, arity);
				p.setPredicateProperty(Predicate.EXPORTED, (String) m
						.get("Exported"));
				p.setPredicateProperty(Predicate.DYNAMIC, (String) m
						.get("Dynamic"));
				p.setPredicateProperty(Predicate.MULTIFILE, (String) m
						.get("Multifile"));
				p.setPredicateProperty(Predicate.MODULE_TRANSPARENT, (String) m
						.get("Transparent"));
				p.setPredicateProperty("summary", (String) m.get("Summary"));

				elms[i++] = p;
			}
		} finally {
			if (session != null) {
				session.dispose();
			}
		}
		return elms;
	}

	public IContextInformation[] computeContextInformation(ITextViewer viewer,
			int offset) {
		// TODO need to integrate pldoc parser
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[] { ':' };
	}

	public char[] getContextInformationAutoActivationCharacters() {
		return new char[] { ':' };
	}

	public String getErrorMessage() {
		return errorMsg;
	}

	public IContextInformationValidator getContextInformationValidator() {
		class Validator implements IContextInformationValidator {

			public boolean isContextInformationValid(int position) {
				return true;
			}

			public void install(IContextInformation info, ITextViewer viewer,
					int documentPosition) {
				;

			}
		}
		return new Validator();
	}

}

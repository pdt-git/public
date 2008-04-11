package org.cs3.pdt.internal.contentassistant;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession2;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.swt.graphics.Image;

public abstract class NaivPrologContentAssistProcessor extends PrologContentAssistProcessor implements
		IContentAssistProcessor {

	@Override
	protected void addVariableProposals(IDocument document, int begin, int len,
			String prefix, List<ComparableCompletionProposal> proposals) throws BadLocationException {

		Set<String> unique = new HashSet<String>();
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
							proposals.add(new VariableCompletionProposal(proposal,
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
							proposals.add(new VariableCompletionProposal(proposal,
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

	@Override
	protected void addPredicateProposals(IDocument document, int begin, int len,
			String prefix, List<ComparableCompletionProposal> proposals, String module)
			throws PrologInterfaceException, CoreException {

		if (PLEditor.isVarPrefix(prefix)) {
			return;
		}
		if(getProject() == null) {
			Debug.warning("Stopped completion proposal creation. No associated Prolog project found for project '" + getFile().getProject().getName() + "'.");
			return;
		}

		PrologInterface pif = getProject().getMetadataPrologInterface();
		PrologSession2 s = null;
		try {
			s = (PrologSession2) pif.getSession();
			s.setPreferenceValue("socketsession.canonical","true");
			/* pdt_completion(File,ContextName,Prefix,ModuleName:PredName/Arity,Tags) */
			IFile file = getFile();
			String path = Util.prologFileName(file.getLocation().toFile());
			String query = "pdt_completion('" + path + "',"
					+ (module != null ? "'" + module + "'" : "_") + ",'"
					+ prefix + "',Module:Name/Arity,Tags)";
			List<Map<String, Object>> l = s.queryAll(query);

			for (Map map : l) {
				ComparableCompletionProposal p = new PredicateCompletionProposal(
						begin, len, /*((CTerm) map.get("Module"))
								.getFunctorValue(), */ ((CTerm) map.get("Name"))
								.getFunctorValue(), Integer
								.parseInt(((CTerm) map.get("Arity"))
										.getFunctorValue()), PLUtil
								.listAsMap((CTerm) map.get("Tags")));

				proposals.add(p);
			}
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

}

package org.cs3.pdt.internal.contentassistant;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.internal.editors.PLPartitionScanner;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.PrologSession2;
import org.eclipse.core.resources.IFile;
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

public abstract class PrologContentAssistProcessor implements
		IContentAssistProcessor {

	protected abstract IFile getFile() throws CoreException;

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
			Collections.sort(proposals);
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

	private boolean isComment(ITypedRegion region) {
		return region.getType().equals(PLPartitionScanner.PL_COMMENT)
				|| region.getType().equals(PLPartitionScanner.PL_MULTI_COMMENT);
	}

	private void addPredicateProposals(IDocument document, int begin, int len,
			String prefix, List proposals, String module)
			throws PrologInterfaceException, CoreException {

		if (PLEditor.isVarPrefix(prefix)) {
			return;
		}
		Predicate[] elems = null;
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
			List<Map> l = s.queryAll(query);

			for (Map map : l) {
				PredicateCompletionProposal p = new PredicateCompletionProposal(
						begin, len, ((CTerm) map.get("Module"))
								.getFunctorValue(), ((CTerm) map.get("Name"))
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

	public IContextInformation[] computeContextInformation(ITextViewer viewer,
			int offset) {

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

	public String getErrorMessage() {
		return "Error Message?";
	}

	protected IPrologProject getProject() throws CoreException {
		IFile file = getFile();
		if (file == null) {
			return null;
		}
		return PDTCoreUtils.getPrologProject(file);
	}

}

package org.cs3.pdt.internal.contentassistant;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.internal.editors.PLPartitionScanner;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterfaceException;
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
		IDocument document;
		String prefix;
				
		Prefix(IDocument document, int begin, String prefix) {
			this.document=document;
			this.begin=begin;
			this.prefix=prefix;
			this.length=prefix.length();
		}
	}
		
	protected abstract void addPredicateProposals(IDocument document, int begin,
			int len, String prefix, List<ComparableCompletionProposal> proposals, String module)
			throws PrologInterfaceException, CoreException;

	protected abstract void addVariableProposals(IDocument document, int begin,
			int len, String prefix, List<ComparableCompletionProposal> proposals) throws BadLocationException, PrologInterfaceException, CoreException;

	private Prefix calculatePrefix(IDocument document, int offset)
			throws BadLocationException {
				int begin=offset;
				while (PLEditor.isNonQualifiedPredicatenameChar(document
						.getChar(begin))
						&& begin > 0)
					begin--;
				int length = offset - begin;
				begin++;
				String pre = document.get(begin, length);
				
				Prefix prefix = new Prefix(document,begin,pre);
				return prefix;
			}

	private String retrievePrefixedModule(int documentOffset, IDocument document, int begin)
			throws BadLocationException {
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

	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
	
		try {
			IDocument document = viewer.getDocument();
	
			documentOffset = documentOffset == 0 ? documentOffset
					: documentOffset - 1;
	
			Prefix pre = calculatePrefix(document,documentOffset);
	
			String module = retrievePrefixedModule(documentOffset - pre.length - 1,
					document, pre.begin);
	
			List<ComparableCompletionProposal> proposals = 
					new ArrayList<ComparableCompletionProposal>();
			if (module == null || module.equals("")) {
				if (pre.prefix.equals("")) {
					return null;
				}
				addVariableProposals(document, pre.begin, pre.length, pre.prefix, proposals);
			}
			addPredicateProposals(document, pre.begin, pre.length, pre.prefix, proposals,
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
package org.cs3.pdt.internal.contentassistant;

import java.util.Map;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.common.Debug;
import org.cs3.pl.cterm.CTerm;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension3;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension5;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

public class PrologCompletionProposal implements Comparable<PrologCompletionProposal>,ICompletionProposal,ICompletionProposalExtension5,ICompletionProposalExtension3, IInformationControlCreator{
	private int offset;
	private int length;
	private String name;	
	private String id;
	private String module;
	private int arity;
	private Map<String,Object> tags;
	public void apply(IDocument document) {
		try {
			document.replace(offset, length, name);
		} catch (BadLocationException e) {
			Debug.report(e);
		}
		
	}

	public String getAdditionalProposalInfo() {
		
		return null;
	}

	public IContextInformation getContextInformation() {
		return null;
	}

	public String getDisplayString() {
		CTerm summary = (CTerm) tags.get("summary");
		return summary!=null?getLabel() + " - " + summary.getFunctorValue():getLabel();
	}

	private String getLabel() {
		String label = name+"/"+arity;
		return label;
	}

	public Image getImage() {
		return ImageRepository.getImage(tags.containsKey("public")?ImageRepository.PE_PUBLIC:ImageRepository.PE_HIDDEN);
	}

	public Point getSelection(IDocument document) {
		return new Point(offset + name.length(), 0);
	}

	public Object getAdditionalProposalInfo(IProgressMonitor monitor) {
		return null;
	}

	public IInformationControlCreator getInformationControlCreator() {
		
		return this;
	}

	public int getPrefixCompletionStart(IDocument document, int completionOffset) {
		return completionOffset;
	}

	public CharSequence getPrefixCompletionText(IDocument document,
			int completionOffset) {

		return getDisplayString();
	}

	public IInformationControl createInformationControl(Shell parent) {
		return new BrowserInformationControl(parent);
	}

	

	public PrologCompletionProposal(int offset, int length, String module,
			String name, int arity, Map<String,Object> tags) {
		this.offset = offset;
		this.length = length;
		this.name=name;
		this.module=module;
		this.arity=arity;
		this.tags=tags;
	}

	

	

	public int compareTo(PrologCompletionProposal o) {
		
		return getLabel().compareTo(o.getLabel());
	}

}

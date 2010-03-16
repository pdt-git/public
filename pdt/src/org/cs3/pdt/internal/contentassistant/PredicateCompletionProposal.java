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
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension3;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension5;
import org.eclipse.swt.widgets.Shell;

public class PredicateCompletionProposal extends ComparableCompletionProposal implements ICompletionProposalExtension5,ICompletionProposalExtension3, IInformationControlCreator{
	private int offset;
	private int length;
	private String name;	
	private Map<String,?> tags;
	private String label;
	
	public PredicateCompletionProposal(int offset, int length,
			String name, int arity, Map<String,?> tags) {
		super(name,offset,length,name.length(),
				ImageRepository.getImage(tags.containsKey("public")?ImageRepository.PE_PUBLIC:ImageRepository.PE_HIDDEN),
						null,null,null);
		this.offset = offset;
		this.length = length;
		this.name=name;
		this.tags=tags;
		this.label = name+"/"+arity;
	}
	
	public void apply(IDocument document) {
		try {
			document.replace(offset, length, name);
		} catch (BadLocationException e) {
			Debug.report(e);
		}
	}

	/**
	 * TRHO: Ugly, summery is generated by pldoc entries, documentation comes for built-ins 
	 */
	public String getDisplayString() {
		CTerm summary = (CTerm) tags.get("summary");
		CTerm doc = (CTerm) tags.get("documentation");
		if(summary!=null){
			return getLabel() + " - " + summary.getFunctorValue();
		 } else if(doc != null){
			 String value = doc.getFunctorValue();
			return value.indexOf('\n')>0 ?
					value.substring(0,value.indexOf('\n')):
						value;
		 }
		return getLabel();
	}

	private String getLabel() {
		return label;
	}

/*	public Point getSelection(IDocument document) {
		return new Point(offset + name.length(), 0);
	}
*/
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
	
	/**
	 * Format needs improvement.
	 */
	public Object getAdditionalProposalInfo(IProgressMonitor monitor) {
		CTerm doc = (CTerm) tags.get("documentation");
		if(doc!=null){
			return doc.getFunctorValue().replaceAll("\n", "</b>");
		}
		CTerm summary = (CTerm) tags.get("summary");
		if(summary!=null){
			return "<b>"+getLabel()+"</b></p>"+summary.getFunctorValue();
		}

		return null;
	}

	public IInformationControlCreator getInformationControlCreator() {
		
		return this;
	}


	@Override
	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof PredicateCompletionProposal)
			return getLabel().compareTo(((PredicateCompletionProposal)o).getLabel());
		return 0;
	}

}

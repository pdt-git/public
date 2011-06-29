package org.cs3.pdt.internal.editors;

import org.cs3.pdt.quickfix.PDTMarker;
import org.cs3.pdt.quickfix.PDTQuickFix;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.texteditor.MarkerAnnotation;

public class PrologCompletionProposal implements ICompletionProposal {
	
	

	private IQuickAssistInvocationContext context;
	private MarkerAnnotation annotation;
	private boolean showWizard;

	public PrologCompletionProposal(IQuickAssistInvocationContext context,
			MarkerAnnotation annotation, boolean showWizard) {
		this.annotation = annotation;
		this.context = context;
		this.showWizard = showWizard;
	}

	@Override
	public void apply(IDocument document) {
		String quickfixDescription = annotation.getMarker().getAttribute(PDTMarker.QUICKFIX_DESCRIPTION, "");
		PDTQuickFix quickFix = new PDTQuickFix(quickfixDescription, showWizard);
		quickFix.run(annotation.getMarker());
	}

	@Override
	public Point getSelection(IDocument document) {
		return null;
	}

	@Override
	public String getAdditionalProposalInfo() {
		return annotation.getMarker().getAttribute(PDTMarker.QUICKFIX_ACTION, "default action");
	}

	@Override
	public String getDisplayString() {
//		String description = annotation.getMarker().getAttribute(PDTMarker.QUICKFIX_DESCRIPTION, "default value") + " - " +annotation.getMarker().getAttribute(PDTMarker.QUICKFIX_ACTION, "");
		String description = annotation.getMarker().getAttribute(PDTMarker.QUICKFIX_DESCRIPTION, "default value");
		if (showWizard) {
			description += " (show preview)";
		}
		
		return description;
	}

	@Override
	public Image getImage() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IContextInformation getContextInformation() {
		// TODO Auto-generated method stub
		return null;
	}

}

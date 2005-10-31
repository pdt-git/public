package org.cs3.pdt.internal.editors;

import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IAutoIndentStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.widgets.Shell;

public class PLConfiguration extends SourceViewerConfiguration {
	private PLDoubleClickStrategy doubleClickStrategy;
//	private PLTagScanner tagScanner;
	private PLScanner scanner;
	private ColorManager colorManager;
	private IContentAssistant assistant;
	
	/* FIXME should not depend on editor
	 * 
	 * 
	 * this was added for because completion, etc. needs some way to 
	 * associate the document its working on with a prolog resource -
	 * it needs to get a prolog helper for that project. 
	 * 
	 * currently, we assume that completion is only used within the 
	 * editor. We can use its input to resolve the connected resource.
	 * -> project -> nature -> plhelper.
	 */
	private PLEditor editor;
	
//	public IContentAssistant getAssistant(){
//		return assistant;
//	}

	public PLConfiguration(ColorManager colorManager, PLEditor editor) {
		this.colorManager = colorManager;
		this.editor = editor;
	}
	
	public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
		return new String[] {
			IDocument.DEFAULT_CONTENT_TYPE,
			PLPartitionScanner.PL_COMMENT,
			PLPartitionScanner.PL_MULTI_COMMENT };
	}
	public ITextDoubleClickStrategy getDoubleClickStrategy(
		ISourceViewer sourceViewer,
		String contentType) {
		if (doubleClickStrategy == null)
			doubleClickStrategy = new PLDoubleClickStrategy();
		return doubleClickStrategy;
	}

	
	protected PLScanner getPLScanner() {
		if (scanner == null) {
			reinitScanner();
		}
		return scanner;
	}

	/**
     * 
     */
    public void reinitScanner() {
        scanner = new PLScanner(editor,colorManager);
        scanner.setDefaultReturnToken(
        	new Token(
        		new TextAttribute(
        			colorManager.getColor(IPLColorConstants.DEFAULT))));
    }

    public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {
		PresentationReconciler reconciler = new PresentationReconciler();

		NonRuleBasedDamagerRepairer ndr =
			new NonRuleBasedDamagerRepairer(
					new TextAttribute(
							colorManager.getColor(IPLColorConstants.PL_COMMENT)));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_MULTI_COMMENT);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_MULTI_COMMENT);
//		DefaultDamagerRepairer dr =
//			new DefaultDamagerRepairer(getPLScanner());
//		reconciler.setDamager(dr, PLPartitionScanner.PL_MULTI_COMMENT);
//		reconciler.setRepairer(dr, PLPartitionScanner.PL_MULTI_COMMENT);

		DefaultDamagerRepairer dr = new DefaultDamagerRepairer(getPLScanner());
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

/*		dr = new DefaultDamagerRepairer(getPLScanner());
		reconciler.setDamager(dr, PLPartitionScanner.PL_DEFAULT);
		reconciler.setRepairer(dr, PLPartitionScanner.PL_DEFAULT);
*/		
		ndr =
			new NonRuleBasedDamagerRepairer(
				new TextAttribute(
					colorManager.getColor(IPLColorConstants.PL_COMMENT)));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_COMMENT);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_COMMENT);

		return reconciler;
	}

	public IAutoIndentStrategy getAutoIndentStrategy(ISourceViewer sourceViewer, String contentType) {
		return new PLAutoIndentStrategy();
	}

	
	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
		if (assistant != null)
			return assistant;
		final ContentAssistant assistant = new PrologContentAssistant();
		assistant.setContentAssistProcessor(new PrologCompletionProcessor(editor),IDocument.DEFAULT_CONTENT_TYPE);
		//assistant.setContentAssistProcessor(new PrologCompletionProcessor(),PLPartitionScanner.PL_COMMENT);
//		assistant.setContentAssistProcessor(new PrologCompletionProcessor(),PLPartitionScanner.PL_MULTI_COMMENT);
		assistant.enableAutoActivation(true);
		//assistant.enableAutoInsert(true);
//		assistant.setAutoActivationDelay(500);
		assistant.setAutoActivationDelay(500);
		assistant.install(sourceViewer);
		assistant.setInformationControlCreator(new IInformationControlCreator() {
            public IInformationControl createInformationControl(Shell parent) {

                return new DefaultInformationControl(parent);
            }
        });
		this.assistant = assistant;

//		StyledText text= sourceViewer.getTextWidget();
//		text.addKeyListener(new KeyListener() {
//			boolean lastKeyCtrl = false;
//			
//			public void keyPressed(KeyEvent e) {
//				//System.out.println("mask: " +e.stateMask + ", data: "+e.data + ", code: "+ e.keyCode +  "\n");
////				if (e.keyCode == 16777227) { //F2
////					assistant.showPossibleCompletions();		
////				}	
//				if (e.keyCode == 16777230) { //F5
//					assistant.showContextInformation();		
//				}	
//				lastKeyCtrl = (e.keyCode == 262144);// CTRL   TODO: simultanous pressed keys are not recognized
//			}
//
//			public void keyReleased(KeyEvent e) {
//			}
//			
//		});
		return assistant;
		
	}
	public String[] getDefaultPrefixes(ISourceViewer sourceViewer, String contentType) {
		return new String[]{"%",""};
	}

}
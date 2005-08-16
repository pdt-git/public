/*
 * Created on 13.04.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal.editors;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.UIUtils;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.PrologException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContextInformation;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IFileEditorInput;

/**
 * @author windeln
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
public class PrologCompletionProcessor implements IContentAssistProcessor {

    public class PrologContextInformation implements IContextInformation {

        private Predicate data;

        public PrologContextInformation(Predicate data) {
            this.data = data;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.text.contentassist.IContextInformation#getContextDisplayString()
         */
        public String getContextDisplayString() {
            // TODO: when is this evaluated?
            //			try {
            //				return getHelp(data);
            //			} catch (IOException e) {
            //				Debug.report(e);
            //				return data.getSignature();
            //			}
            return "";
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.text.contentassist.IContextInformation#getImage()
         */
        public Image getImage() {
            // TODO Auto-generated method stub
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.text.contentassist.IContextInformation#getInformationDisplayString()
         */
        public String getInformationDisplayString() {
            String help;
            try {
                help = getHelp(data);
                IContextInformation context = null;
                if (help != null && help.length() > 0)
                    return help.substring(data.getLabel().length(), help
                            .indexOf('\n'));
            } catch (IOException e) {
                Debug.report(e);
            }
            return "";
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeCompletionProposals(org.eclipse.jface.text.ITextViewer,
     *           int)
     */
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
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }

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

    /**
     * @param document
     * @param begin
     * @param len
     * @param prefix
     * @param proposals
     * @throws BadLocationException
     */
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
                if (isComment(region))
                    l = region.getOffset() + region.getLength();
                else {
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

    /**
     * @param region
     * @return
     */
    private boolean isComment(ITypedRegion region) {
        return region.getType().equals(PLPartitionScanner.PL_COMMENT)
                || region.getType().equals(PLPartitionScanner.PL_MULTI_COMMENT);
    }

    //	/**
    //	 * @param begin
    //	 * @param len
    //	 * @param prefix
    //	 * @param proposals
    //	 */
    //	private void addPredicateProposals(int begin, int len, String prefix,
    // List proposals, Set unique) throws IOException {
    //		if (PLEditor.isVarPrefix(prefix))
    //				return ;
    //		PrologElementData[] elems =
    // PrologManager.getInstance().getHiddenClient().retrievePrologElements(PDTPlugin.getDefault().getActiveFileName());//getCachedPrologElements(PDTPlugin.getDefault().getActiveFileName());
    //		if (elems == null){
    //			PDTPlugin.message("Could not find predicates for this file. Please save
    // the file first.");
    //			return ;
    //		}
    //		addProposals(begin, len, prefix, proposals, elems,unique);
    //	}

    /**
     * @param document
     * @param begin
     * @param len
     * @param prefix
     * @param proposals
     */
    private void addPredicateProposals(IDocument document, int begin, int len,
            String prefix, List proposals, String module) throws IOException {

        //		if(module == null)
        //			addPredicateProposals(begin, len, prefix, proposals,unique );
        if (PLEditor.isVarPrefix(prefix))
            return;
        //		if(PrologManager.getInstance().getClient().isInCall()) {
        //			String msg = "Code assist cannot access the Prolog System, because
        // the console may run in debug mode.";
        //			PDTPlugin.getDefault().setStatusErrorMessage(msg);
        //			throw new IOException(msg);
        //		}

        
        IMetaInfoProvider prologHelper = PDTCorePlugin.getDefault().getMetaInfoProvider();
        IFileEditorInput editorInput = (IFileEditorInput) UIUtils
                .getActiveEditor().getEditorInput();
        String activeFileName = editorInput.getFile().getFullPath().toString();
        Predicate[] elems = null;
        ;
        try {
            elems = prologHelper.getPredicatesWithPrefix(module, prefix,
                    activeFileName);
        } catch (PrologException e) {
            Debug.report(e);
            elems = new Predicate[0];
        }
        addProposals(begin, len, prefix, proposals, elems);
    }

    /**
     * @param begin
     * @param len
     * @param prefix
     * @param proposals
     * @param elems
     * @return
     */
    private List addProposals(int begin, int len, String prefix,
            List proposals, Predicate[] elems) throws IOException {
        Set unique = new HashSet();

        for (int i = 0; i < elems.length; i++) {
            if (!unique.contains(elems[i].getSignature())) {
                unique.add(elems[i].getSignature());
                ICompletionProposal proposal = new PrologCompletionProposal(
                        elems[i], begin, len, prefix);
                proposals.add(proposal);
            }
            /*
             * if(elems[i].getLabel().regionMatches(true,0,prefix,0,prefix.length()) &&
             * !unique.contains(elems[i].getSignature())) {
             * unique.add(elems[i].getSignature()); String postfix = ""; int
             * cursorPos = elems[i].getLabel().length(); if (elems[i].getArity() >
             * 0) { postfix = "()"; cursorPos++; } else if (elems[i].getArity() ==
             * -1) { postfix = ":"; cursorPos++; }
             * 
             * String help = getHelp(elems[i]); IContextInformation context =
             * null; if (help != null && help.length() > 0) { int predLen =
             * elems[i].getLabel().length(); int firstLB = help.indexOf('\n');
             * if(firstLB > predLen) { String params =
             * help.substring(predLen,firstLB); context = new
             * ContextInformation(null, "", params ); } } Image img =
             * elems[i].isPublic() ? image : hidden; proposals.add(new
             * CompletionProposal(elems[i].getLabel() +
             * postfix,begin,len,cursorPos,img, elems[i].getSignature(), context
             * new PrologContextInformation(elems[i]),help )); }
             */

        }
        return proposals;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeContextInformation(org.eclipse.jface.text.ITextViewer,
     *           int)
     */
    public IContextInformation[] computeContextInformation(ITextViewer viewer,
            int documentOffset) {
        try {
            String help = getHelp(viewer, documentOffset);
            if (help == null)
                return null;
            IContextInformation info = new ContextInformation(null, "context1",
                    help);
            return new IContextInformation[] { info };
        } catch (BadLocationException e) {
            Debug.report(e);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            Debug.report(e);
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }
        return null;
    }

    /**
     * @param data
     * @return
     * @throws IOException
     */
    private String getHelp(Predicate data) throws IOException {
        
		
		IMetaInfoProvider metaInfoProvider = PDTCorePlugin.getDefault().getMetaInfoProvider();
		
        return metaInfoProvider.getHelp(data);
    }

    /**
     * @param viewer
     * @param documentOffset
     * @return
     * @throws BadLocationException
     * @throws IOException
     */
    private String getHelp(ITextViewer viewer, int documentOffset)
            throws BadLocationException, IOException {
        Goal data = PLEditor.getPrologDataFromOffset(viewer
                .getDocument(), documentOffset);
        if (data == null){
			return null;
        }
		
		IMetaInfoProvider metaInfoProvider = PDTCorePlugin.getDefault().getMetaInfoProvider();
		Predicate[] preds = metaInfoProvider.findPredicates(data);
		//FIXME: what about alternatives?
		if(preds==null||preds.length==0){
			return null;
		}
        String help = metaInfoProvider.getHelp(preds[0]);
		if(preds.length>1){
			help+="\n--------\n NOTE: I found more than one predicate matching the signature" +
					" '"+data+"'\n" +
							"Sorry, but code analysis is still work in progress. For now i will" +
							"simply ignore all but the first match";
		}
		return help;

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getCompletionProposalAutoActivationCharacters()
     */
    public char[] getCompletionProposalAutoActivationCharacters() {
        // TODO Auto-generated method stub
        return new char[] { ':' };//new char[]{'\t'};
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationAutoActivationCharacters()
     */
    public char[] getContextInformationAutoActivationCharacters() {
        // TODO Auto-generated method stub
        return new char[] { ':' };//null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getErrorMessage()
     */
    public String getErrorMessage() {
        // TODO Auto-generated method stub
        return "ERROR in Completion";
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationValidator()
     */
    public IContextInformationValidator getContextInformationValidator() {
        try {

            class Validator implements IContextInformationValidator {

                IDocument document;

                public boolean isContextInformationValid(int position) {
                    //				try {
                    //					PLEditor.getPrologDataFromOffset(document, position);
                    //				} catch (BadLocationException e) {
                    //					PDTPlugin.getDefault().setStatusErrorMessage("Can not
                    // find a
                    // valid predicate");
                    //					return false;
                    //				}
                    return true;
                }

                /*
                 * (non-Javadoc)
                 * 
                 * @see org.eclipse.jface.text.contentassist.IContextInformationValidator#install(org.eclipse.jface.text.contentassist.IContextInformation,
                 *           org.eclipse.jface.text.ITextViewer, int)
                 */
                public void install(IContextInformation info,
                        ITextViewer viewer, int documentPosition) {
                    document = viewer.getDocument();

                }
            }
            ;
            return new Validator();
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }
}

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

import java.util.Map;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.contentassist.ContextInformation;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension3;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension5;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;



public class NewPrologCompletionProposal implements ICompletionProposal,ICompletionProposalExtension5,ICompletionProposalExtension3, IInformationControlCreator {
	
	private PrologInterface pif;
	private Predicate predicate;
	private int replacementOffset;
	private int replacementLength;
	private String prefix;
	private int cursorPosition;
	private String postfix;
	private Image image;
	private IContextInformation contextInfo;
	
	private static final Image publicImage = ImageRepository.getImage(ImageRepository.PE_PUBLIC);
    private static final Image hiddenImage = ImageRepository.getImage(ImageRepository.PE_HIDDEN);
    
	public NewPrologCompletionProposal(PrologInterface pif,Predicate predicate, int begin, int len, String prefix) {
		this.pif=pif;
		this.predicate=predicate;
		this.replacementOffset=begin;
		this.replacementLength=len;
		this.prefix=prefix;
		if(predicate.getName().regionMatches(true,0,prefix,0,prefix.length()) ) {
			
			postfix = "";
            int cursorPos = predicate.getName().length();
			if (predicate.getArity() > 0) {
				postfix = "()";
				cursorPos++;
			}
			else if (predicate.getArity() == -1) {
				postfix = ":";
				cursorPos++;
			}
			
			
			image = predicate.isPublic() ? publicImage : hiddenImage;
            cursorPosition=cursorPos;
			
	
			
	}
	}

	public void apply(IDocument document) {
		try {
			document.replace(replacementOffset, replacementLength, predicate.getName() + postfix);
		} catch (BadLocationException x) {
			Debug.report(x); 
			//not rethrown. I think it's harmless.
		}

	}

	public Point getSelection(IDocument document) {
		return new Point(replacementOffset + cursorPosition, 0);
		
	}

	public String getAdditionalProposalInfo() {
		return "bla";
	}

	public String getDisplayString() {
		String summary=predicate.getPredicateProperty("summary");
		return predicate.getName()+"/"+predicate.getArity()+"  ("+predicate.getModule()+") - "+ summary;
	}

	public Image getImage() {
		
		return image; 
	}

	public IContextInformation getContextInformation() {
//		if(contextInfo==null){
//			String summary=predicate.getPredicateProperty("summary");
//			contextInfo= new ContextInformation(summary,summary);
//		}
//		return contextInfo;
		return null;
	}

	public Object getAdditionalProposalInfo(IProgressMonitor monitor) {
		
		monitor.beginTask("fetching help",IProgressMonitor.UNKNOWN);
		PrologSession s = null;
		Map map = null;
		try {
			s= pif.getSession();	
			map = s.queryOnce("pdt_builtin_help("+predicate.getName()+","+predicate.getArity()+",_,Help)");
			
		} catch (PrologInterfaceException e) {
			Debug.report(e);
			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
					.getErrorMessageProvider(), UIUtils.getDisplay().getActiveShell(), PDT.ERR_PIF, PDT.CX_COMPLETION, e);
			return null;
		}
		monitor.done();
		return map.get("Help");
	}

	public IInformationControlCreator getInformationControlCreator() {

		return this;
	}

	public int getPrefixCompletionStart(IDocument document, int completionOffset) {
		return completionOffset;
	}

	public CharSequence getPrefixCompletionText(IDocument document, int completionOffset) {
		return getDisplayString();
	}

	public IInformationControl createInformationControl(Shell parent) {
		
		return new BrowserInformationControl(parent);
	}

}

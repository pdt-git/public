/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.console.internal.views.completion;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.parser.ParserDelegator;

import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;

public class PredicateCompletionProposal extends ComparableCompletionProposal {

	private String signature;
	private String label;
	
	private String term;
	private String indicator;
	private String functor;
	
	private String visibility;
	private boolean isBuiltin;
	
	private String docKind;
	private String doc;
	private String description;
	private boolean descriptionSet;
	
	private int lastStateMask = -1;
	
	public PredicateCompletionProposal(String module, String functor, int arity, int prefixLength, String visibility, boolean isBuiltin, List<String> argNames, String docKind, String doc, boolean addSingleQuote) {
		super(prefixLength, addSingleQuote);
		this.visibility = visibility;
		this.isBuiltin = isBuiltin;
		signature = functor + "/" + arity;
		if (module == null) {
			label = signature;
		} else {
			label = signature + " - " + module;
		}
		term = (functor + (addSingleQuote ? "'" : "") + getArglist(arity, argNames)).substring(prefixLength);
		indicator = (functor + (addSingleQuote ? "'" : "") + "/" + arity).substring(prefixLength);
		this.functor = functor + (addSingleQuote ? "'" : "");
		this.docKind = docKind;
		this.doc = doc;
	}
	
	@Override
	public String getContent(int stateMask) {
		lastStateMask = stateMask;
		if ((stateMask & SWT.CTRL) != 0) {
			return indicator;
		} else if ((stateMask & SWT.SHIFT) != 0) {
			return functor.substring(prefixLength);
		} else {
			return term;
		}
	}

	private String getArglist(int arity, List<String> argNames) {
		if (arity < 1) {
			return "";
		}
		
		StringBuffer buf = new StringBuffer("(");
		char c = 'A';
		int i = 0;
		
		if (argNames == null) {
			while (i < arity) {
				if (i > 0) {
					buf.append(", ");
				}
				buf.append(c);
				if (c == '_' || c == 'Z') {
					c = '_';
				} else {
					c++;
				}
				i++;
			}
		} else {
			for (String argName : argNames) {
				if (i > 0) {
					buf.append(", ");
				}
				buf.append(argName);
				i++;
			}
		}
		buf.append(")");
		return buf.toString();
	}
	
	@Override
	public int getCursorPosition() {
		return getContent(lastStateMask).length();
	}

	@Override
	public String getLabel() {
		return label;
	}

	@Override
	public String getDescription() {
		if (!descriptionSet) {
			if (SearchConstants.COMPLETION_DOC_KIND_TEXT.equals(docKind)) {
				description = doc;
			} else if (SearchConstants.COMPLETION_DOC_KIND_HTML.equals(docKind)) {
				description = extractTextFromHtml(doc.trim());
			} else if (SearchConstants.COMPLETION_DOC_KIND_FILE.equals(docKind)) {
				String fileContent = Util.readFromFile(new File(doc));
				if (fileContent != null && !fileContent.isEmpty()) {
					description = fileContent;
				}
			} else if (SearchConstants.COMPLETION_DOC_KIND_LGT_HELP_FILE.equals(docKind)) {
				String fileContent = Util.readFromFile(new File(doc));
				if (fileContent != null && !fileContent.isEmpty()) {
					return extractTextFromHtml(fileContent.substring(fileContent.indexOf("<body>") + 6, fileContent.indexOf("</body>")));
				}
			}
			descriptionSet = true;
		}
		return description;
	}
	
	@Override
	public Image getImage() {
		if (isBuiltin) {
			return ImageRepository.getImage(ImageRepository.PREDICATE_BUILTIN);
		} else {
			if (SearchConstants.VISIBILITY_PUBLIC.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PUBLIC);
			} else if (SearchConstants.VISIBILITY_PROTECTED.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PROTECTED);
			} else if (SearchConstants.VISIBILITY_PRIVATE.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PRIVATE);
			}
		}
		return null;
	}
	
	String getSignature() {
		return signature;
	}

	@Override
	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof PredicateCompletionProposal) {
			return getSignature().compareTo(((PredicateCompletionProposal) o).getSignature());
		} else if (o instanceof ModuleCompletionProposal) {
			return getSignature().compareTo(o.getLabel());
		} else {
			return -1;
		}
	}
	
	private String extractTextFromHtml(final String html) {
		final StringBuilder sb = new StringBuilder();
		HTMLEditorKit.ParserCallback parserCallback = new HTMLEditorKit.ParserCallback() {
		    public boolean readyForNewline;

		    @Override
		    public void handleText(final char[] data, final int pos) {
		        String s = new String(data);
		        sb.append(s);
		        readyForNewline = true;
		    }

		    @Override
		    public void handleStartTag(final HTML.Tag t, final MutableAttributeSet a, final int pos) {
		        if (readyForNewline && (t == HTML.Tag.DIV || t == HTML.Tag.BR || t == HTML.Tag.P || t == HTML.Tag.DD || t == HTML.Tag.DT)) {
		            sb.append("\n");
		            readyForNewline = false;
		        }
		    }

		    @Override
		    public void handleSimpleTag(final HTML.Tag t, final MutableAttributeSet a, final int pos) {
		        handleStartTag(t, a, pos);
		    }
		};
		try {
			new ParserDelegator().parse(new StringReader(html), parserCallback, false);
		} catch (IOException e) {
			Debug.report(e);
		}
		return sb.toString();
	}

}



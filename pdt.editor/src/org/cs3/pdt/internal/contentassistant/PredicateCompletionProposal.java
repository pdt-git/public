package org.cs3.pdt.internal.contentassistant;

import java.util.List;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.internal.text.html.BrowserInformationControl;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension5;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;

@SuppressWarnings({ "unused", "restriction" })
public class PredicateCompletionProposal extends ComparableTemplateCompletionProposal implements ICompletionProposalExtension5, IInformationControlCreator {

	public static PredicateCompletionProposal createProposal(IDocument document, int offset, int length, String module, String name, int arity, List<String> argNames, String visibility, boolean isBuiltin, String docKind, String doc) {
		String insertion = createInsertion(name, arity, argNames);
		String displayString = createDisplayString(module, name, arity);
		return new PredicateCompletionProposal(document, insertion, displayString, offset, length, module, name, arity, visibility, isBuiltin, argNames, docKind, doc);
	}
	
	private static String createDisplayString(String module, String name, int arity)  {
		if (module == null) {
			return name + "/" + arity;
		} else {
			return name + "/" + arity + " - " + module;
		}
	}
	
	private static String createInsertion(String name, int arity, List<String> argNames) {
		if (arity <= 0) {
			return name;
		}
		boolean createArglist = Boolean.parseBoolean(PDTPlugin.getDefault().getPreferenceValue(PDT.PREF_AUTO_COMPLETE_ARGLIST, "true"));
		if (!createArglist) {
			return name;
		}
		StringBuffer buf = new StringBuffer(name);
		buf.append("(");
		if (argNames == null) {
			for (int i = 0; i < arity; i++) {
				if (i == 0) {
					buf.append("${_");
				} else {
					buf.append(", ${_");
				}
				buf.append(i);
				buf.append("}");
			}
		} else {
			boolean first = true;
			for (String argName : argNames) {
				if (first) {
					first = false;
					buf.append("${");
					buf.append(argName);
					buf.append("}");
				} else {
					buf.append(", ${");
					buf.append(argName);
					buf.append("}");
				}
			}
		}
		buf.append(")");
		return buf.toString();
	}

	private String module;
	private String name;
	private int arity;
	private String docKind;
	private String doc;
	private List<String> argNames; 
	private String visibility;
	private boolean isBuiltin;
	
	private PredicateCompletionProposal(IDocument document, String insertion, String displayString, int offset, int length, String module, String name, int arity, String visibility, boolean isBuiltin, List<String> argNames, String docKind, String doc) {
		super(document, insertion, displayString, offset, length);
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.visibility = visibility;
		this.isBuiltin = isBuiltin;
		this.argNames = argNames;
		this.docKind = docKind;
		this.doc = doc;
	}

	@Override
	public int compareTo(ComparableTemplateCompletionProposal o) {
		if (o instanceof VariableCompletionProposal) {
			return -1;
		} else if (o instanceof PredicateCompletionProposal) {
			return getSignature().compareTo(((PredicateCompletionProposal) o).getSignature());
		} else if (o instanceof ModuleCompletionProposal) {
			return getSignature().compareTo(o.getDisplayString());
		} else {
			return 1;
		}
	}

	@Override
	public Object getAdditionalProposalInfo(IProgressMonitor monitor) {
		if (SearchConstants.COMPLETION_DOC_KIND_NODOC.equals(docKind)) {
			return null;
		} else if (SearchConstants.COMPLETION_DOC_KIND_TEXT.equals(docKind)) {
			return doc;
		} else if (SearchConstants.COMPLETION_DOC_KIND_HTML.equals(docKind)) {
			if (doc != null) {
				if(doc.indexOf("\n") > -1){
					doc="<b>"+doc.replaceFirst("\n", "</b><br/>").replace("\n", "<br/>");
				}
				return doc;
			} else {
				return null;
			}
		} else {
			return null;
		}
	}
	
	@Override
	public IInformationControlCreator getInformationControlCreator() {
		return this;
	}

	@Override
	public IInformationControl createInformationControl(Shell parent) {
		if (BrowserInformationControl.isAvailable(parent)) {
			return new BrowserInformationControl(parent, JFaceResources.DIALOG_FONT, true) {
				@Override
				public IInformationControlCreator getInformationPresenterControlCreator() {
					return PredicateCompletionProposal.this;
				}
			};
		} else {
			return new DefaultInformationControl(parent);
		}
	}
	
	@Override
	public Image getImage() {
		if (isBuiltin) {
			return ImageRepository.getImage(ImageRepository.PE_BUILT_IN);
		} else {
			if (SearchConstants.VISIBILITY_PUBLIC.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
			} else if (SearchConstants.VISIBILITY_PROTECTED.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
			} else if (SearchConstants.VISIBILITY_PRIVATE.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PE_PRIVATE);
			}
		}
		return null;
	}
	
	String getSignature() {
		return name + "/" + arity;
	}
	
}

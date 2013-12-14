package org.cs3.pdt.common.structureElements;

import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.viewers.StyledString;

public class PrologReferenceMatch extends PrologMatch {

	public PrologReferenceMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int line, String declOrDef, String signature) {
		super(searchMatchElement, visibility, module, name, arity, properties, file, line, declOrDef, signature);
	}

	public PrologReferenceMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int offset, int length, String declOrDef, String signature) {
		super(searchMatchElement, visibility, module, name, arity, properties, file, offset, length, declOrDef, signature);
	}
	
	@Override
	protected StyledString createLabel() {
		StyledString str = new StyledString();
		String text = "";
		int line = getLine();
		if (isLineLocation()) {
			IDocument document = getDocument();
			if (document != null && "true".equals(PDTCommonUtil.getProperty(SearchConstants.PROPERTY_SHOW_LINE, getProperties()))) {
				try {
					IRegion lineInformation = document.getLineInformation(line - 1);
					text = document.get(lineInformation.getOffset(), lineInformation.getLength());
				} catch (BadLocationException e) {
					Debug.report(e);
				}
			} else {
				String labelProperty = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_LABEL, getProperties());
				if (labelProperty != null) {
					text = Util.unquoteAtom(labelProperty);
				}
			}
		} else {
			try {
				text = getDocument().get(getOffset(), getLength());
			} catch (BadLocationException e) {
				Debug.report(e);
			}
			text = text.replaceAll("\n|\r", "");
		}
		String prefix = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_PREFIX, getProperties());
		String suffix = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_SUFFIX, getProperties());
		
		if (prefix != null && !prefix.isEmpty()) {
			str.append(Util.unquoteAtom(prefix), StyledString.QUALIFIER_STYLER);
		}
		str.append(text);
		if (suffix != null && !suffix.isEmpty()) {
			str.append(Util.unquoteAtom(suffix), StyledString.DECORATIONS_STYLER);
		}
		return str;
	}

}

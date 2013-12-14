package org.cs3.pdt.common.structureElements;

import java.util.List;

import org.cs3.prolog.common.logging.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.viewers.StyledString;

public class PrologDefinitionMatch extends PrologMatch {

	public PrologDefinitionMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int line, String declOrDef, String signature) {
		super(searchMatchElement, visibility, module, name, arity, properties, file, line, declOrDef, signature);
	}

	public PrologDefinitionMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int offset, int length, String declOrDef, String signature) {
		super(searchMatchElement, visibility, module, name, arity, properties, file, offset, length, declOrDef, signature);
	}
	
	@Override
	protected StyledString createLabel() {
		StyledString str;
		if (isLineLocation()) {
			StringBuilder buf = new StringBuilder("Line ");
			buf.append(Integer.toString(getLine()));
			buf.append(" (");
			buf.append(getDeclOrDef());
			buf.append(")");
			str = new StyledString(buf.toString());
		} else {
			String text = "";
			try {
				text = getDocument().get(getOffset(), getLength());
			} catch (BadLocationException e) {
				Debug.report(e);
			}
			str = new StyledString(getLine() + ": ", StyledString.QUALIFIER_STYLER);
			str.append(text);
		}
		return str;
	}

}

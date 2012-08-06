package org.cs3.pdt.common;

import java.io.File;

import org.cs3.prolog.common.Util;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.part.FileEditorInput;

public class CommonUtil {
	
	private static final String SPAN_HIDDEN = "<span style={display:none}>";
	private static final String NO_DOCUMENTATION = "NO_DOC";
	
	public static String[] getPredicateArgNamesFromDocumentation(String doc) {
		String[] names = null;
		if (doc != null && !doc.equals(NO_DOCUMENTATION)) {
			if (doc.contains("<dt class=\"pubdef\">")){
				if (doc.indexOf("arglist") > 0 && doc.indexOf("</var>") > doc.indexOf("arglist")) {
					String commaSeparatedArgs = doc.substring(doc.indexOf("arglist") + 10, doc.indexOf("</var>") -1);
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\?", "");
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\-", "");
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\+", "");
					names = commaSeparatedArgs.split(",");
					for (int i = 0; i < names.length; i++) {
						int typeSeparator = names[i].indexOf(':');
						if (typeSeparator >= 0) {
							names[i] = names[i].substring(0, typeSeparator);
						}
						names[i] = names[i].trim();
					}
				}
			} else if (doc.indexOf(SPAN_HIDDEN) > 0 && doc.indexOf("</span>") > 0) {
				String head = doc.substring(doc.indexOf(SPAN_HIDDEN) + SPAN_HIDDEN.length(), doc.indexOf("</span>"));
				int indexOfOpeningBracket = head.indexOf("(");
				if (indexOfOpeningBracket != -1) {
					names = head.substring(indexOfOpeningBracket + 1, head.lastIndexOf(")")).split(",");
				}
			}
		}
		return names;
	}
	
	public static String prologFileName(IEditorInput input) {
		if (input instanceof FileEditorInput) {
			FileEditorInput fileEditorInput = (FileEditorInput)input;
			IPath path = fileEditorInput.getPath();
			File file = path.toFile();
			return Util.prologFileName(file);
		}
		if (input instanceof FileStoreEditorInput) {
			FileStoreEditorInput e = (FileStoreEditorInput)input;
			File file = new File(e.getURI());
			return Util.prologFileName(file);
		}
		return input.getName();
	}

}

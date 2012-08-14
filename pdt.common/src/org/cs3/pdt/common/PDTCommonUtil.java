package org.cs3.pdt.common;

import java.io.File;

import org.cs3.pdt.common.metadata.SourceLocation;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.part.FileEditorInput;

public class PDTCommonUtil {
	
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

	public static void showSourceLocation(final SourceLocation loc) {
		if (Display.getCurrent() != UIUtils.getDisplay()) {
	
			UIUtils.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					showSourceLocation(loc);
				}
			});
			return;
		}
		
		// For predicates implemented by external language code the Prolog side returns
		// an error message instead of a file name. Intercept and display it:
		if (loc.file.equals("No Prolog source code (only compiled external language code)")) {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			UIUtils.displayMessageDialog(
					shell,
					"External language predicate",
					"There is no Prolog source code for this predicate (only compiled external language code).");
			return;
		}
		
		if (loc.isLineLocation()) {
			try {
				UIUtils.selectInEditor(loc.getLine(), loc.file, true);
			} catch (PartInitException e) {
				Debug.report(e);
			}
		} else {
			try {
				UIUtils.selectInEditor(loc.getOffset(), loc.getEndOffset() - loc.getOffset(), loc.file, true);
			} catch (PartInitException e) {
				Debug.report(e);
			}
		}
		
		
	}

}

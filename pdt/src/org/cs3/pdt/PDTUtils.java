/* $LICENSE_MSG$(ld) */

package org.cs3.pdt;

import java.io.IOException;

import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.metadata.SourceLocation;
import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

public final class PDTUtils {

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
				IEditorPart editorPart = UIUtils.openInEditor(FileUtils.findFileForLocation(loc.file), true);
				if (editorPart != null && editorPart instanceof PLEditor){
					((PLEditor) editorPart).gotoLine(loc.getLine());
				}
			} catch (PartInitException e) {
				Debug.report(e);
			} catch (IOException e) {
				Debug.report(e);
			}
			
		} else {
			try {
				UIUtils.selectInPrologEditor(loc.getOffset(), loc.getEndOffset() - loc.getOffset(), loc.file);
			} catch (PartInitException e) {
				Debug.report(e);
			}
		}
		
		
	}

	public static PrologInterface getActivePif() {
		return PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();
	}
	
	public static String getPrologFileName(IFile file) {
		String enclFile = file.getRawLocation().toPortableString();
		if (Util.isWindows()) {
			enclFile = enclFile.toLowerCase();
		}

		IPath filepath = new Path(enclFile);
		return Util.prologFileName(filepath.toFile());
	}

}


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

package org.cs3.pdt;

import java.io.IOException;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.metadata.SourceLocation;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

public final class PDTUtils {


	public static boolean checkForActivePif(boolean showDialog) {
		PrologInterface pif = getActiveConsolePif();
		if (pif == null) {
			if (showDialog) {
				UIUtils.displayErrorDialog(Display.getCurrent().getActiveShell(), "No Prolog Process selected", "You have to select an active Prolog process.");
			}
			return false;
		}
		return true;
	}
	
	public static PrologInterface getActiveConsolePif() {
		PrologConsole activePrologConsole = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
		if (activePrologConsole != null) {
			return activePrologConsole.getPrologInterface();
		}
		return null;
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

}
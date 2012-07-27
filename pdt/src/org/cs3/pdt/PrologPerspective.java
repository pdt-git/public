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

package org.cs3.pdt;

import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

public class PrologPerspective implements IPerspectiveFactory {

	@Override
	public void createInitialLayout(IPageLayout layout) {
		defineActions(layout);
		defineLayout(layout);
	}

	public void defineActions(IPageLayout layout) {
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");
	}

	public void defineLayout(IPageLayout layout) {
		String editorArea = layout.getEditorArea();

		layout.addView(JavaUI.ID_PACKAGES, IPageLayout.LEFT, (float) 0.2, editorArea);
		layout.addView("org.cs3.pdt.console.internal.views.PrologConsoleView", IPageLayout.BOTTOM, (float) 0.65, editorArea);
		layout.addView(IPageLayout.ID_OUTLINE, IPageLayout.RIGHT, (float) 0.8, editorArea);
		
//		layout.addView(PrologConsoleView.HOOK_ID, IPageLayout.BOTTOM, (float) 0.65, editorArea);
//		layout.addView("pdt.view.focus", IPageLayout.RIGHT, (float) 0.5, PrologConsoleView.HOOK_ID);
//		layout.addView(JavaUI.ID_PACKAGES, IPageLayout.LEFT, (float) 0.2, editorArea);
//		layout.addView(IPageLayout.ID_OUTLINE, IPageLayout.RIGHT, (float) 0.8, editorArea);
	}
	
}



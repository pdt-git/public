package org.cs3.pdt;

import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

public class PrologPerspective implements IPerspectiveFactory {

	@Override
	public void createInitialLayout(IPageLayout layout) {
		defineActions(layout);
		defineLayout(layout);
	}
	
	public void defineActions(IPageLayout layout) {
		// Add "new wizards".
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");

	}
	
	public void defineLayout(IPageLayout layout) {
        // Editors are placed for free.
        String editorArea = layout.getEditorArea();

        IFolderLayout left = layout.createFolder("left", IPageLayout.LEFT, (float) 0.2, editorArea);
        left.addView(JavaUI.ID_PACKAGES);
        IFolderLayout bottom = layout.createFolder("bottom", IPageLayout.BOTTOM, (float) 0.7, editorArea);
        bottom.addView("org.cs3.pdt.console.internal.views.PrologConsoleView");
        IFolderLayout right = layout.createFolder("right", IPageLayout.RIGHT, (float) 0.8, editorArea);
        right.addView(IPageLayout.ID_OUTLINE);
}
}

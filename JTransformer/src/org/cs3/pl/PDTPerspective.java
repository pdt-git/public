/*
 * Created on 28.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 * 
 * @todo: name not correct
 */
public class PDTPerspective implements IPerspectiveFactory {
	public void createInitialLayout(IPageLayout layout) {
		defineActions(layout);
		defineLayout(layout);
	}
	/**
	 * Defines the initial actions for a page.  
	 */
	public void defineActions(IPageLayout layout) {
		// Add "new wizards".
//		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");//$NON-NLS-1$
//		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");//$NON-NLS-1$

		// Add "show views".
		layout.addShowViewShortcut("org.cs3.pl.views.PrologConsole");
		
		layout.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET);
	}
	/**
	 * Defines the initial layout for a page.  
	 */
	public void defineLayout(IPageLayout layout) {
		// Editors are placed for free.
		String editorArea = layout.getEditorArea();

		// Top left.
		IFolderLayout topLeft = layout.createFolder("topLeft", IPageLayout.LEFT, (float)0.26, editorArea);//$NON-NLS-1$
		topLeft.addView(IPageLayout.ID_RES_NAV);
		topLeft.addPlaceholder(IPageLayout.ID_BOOKMARKS);

		// Bottom left.
		IFolderLayout bottomLeft = layout.createFolder("bottomLeft", IPageLayout.BOTTOM, (float)0.50,//$NON-NLS-1$
		"topLeft");//$NON-NLS-1$
		bottomLeft.addView(IPageLayout.ID_OUTLINE);

		// Bottom right.
		layout.addView(IPageLayout.ID_TASK_LIST, IPageLayout.BOTTOM, (float)0.66, editorArea);
	}
	

}

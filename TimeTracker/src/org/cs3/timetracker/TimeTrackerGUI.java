/*
 * Created on 30.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.timetracker;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.help.WorkbenchHelp;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.model.WorkbenchAdapter;

import org.eclipse.*;

/**
 * @author schmitzs
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TimeTrackerGUI extends ViewPart{
	
///////////////////////////////////
// PRIVATE MEMBER VARIABLES
	TimeTrackerGUIInteraction tti;
	//TimeTrackerV
	
	
	
    /**
     * Creates a new ReadmeSectionsView .
     */
    public TimeTrackerGUI() {
        super();
    }

    /* (non-Javadoc)
     * Method declared on IWorkbenchPart
     */
    public void createPartControl(Composite parent) {
       tti = new TimeTrackerGUIInteraction(parent);
       
    }

    /**
     * The <code>ReadmeSectionView</code> implementation of this 
     * <code>IWorkbenchPart</code> method runs super
     * and removes itself from the global selection listener. 
     */
    public void dispose() {
        super.dispose();
        //getSite().getPage().removeSelectionListener(this);
    }

    /* (non-Javadoc)
     * Method declared on ISelectionListener
     */
/*    public void selectionChanged(IWorkbenchPart part, ISelection sel) {
        //if the selection is a readme file, get its sections.
        AdaptableList input = ReadmeModelFactory.getInstance().getSections(sel);
        viewer.setInput(input);
    }
*/
    /* (non-Javadoc)
     * Method declared on IWorkbenchPart
     */
    public void setFocus() {
        //viewer.getControl().setFocus();
    }
	


}

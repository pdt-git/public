package org.cs3.timetracker.views;


import java.io.File;
import java.io.BufferedReader;

import java.io.FileReader;

import org.cs3.timetracker.ITimeObserver;
import org.cs3.timetracker.TimeEvent;
import org.cs3.timetracker.Logger;
import org.cs3.timetracker.TimeTicker;
import org.cs3.timetracker.TimeTrackerGUIInteraction;
import org.cs3.timetracker.TimeTrackerPlugin;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
//import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 */

public class TimeTrackerGUIViewer extends ViewPart implements ITimeObserver{
	private TableViewer viewer;
	

	/*
	 * The content provider class is responsible for
	 * providing objects to the view. It can wrap
	 * existing objects in adapters or simply return
	 * objects as-is. These objects may be sensitive
	 * to the current input of the view, or ignore
	 * it and always show the same content 
	 * (like Task List, for example).
	 */
	
	private TimeEvent currentTimeEvent;


	private Composite composite;
	private String content = "";
	private Logger log;


	private TimeTrackerGUIInteraction guiInteraction;
	 
	class ViewContentProvider implements IStructuredContentProvider {
		
		public ViewContentProvider(){			
			System.out.println("DEBUG");
		}
		
		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		}
		public void dispose() {
		}
		public Object[] getElements(Object parent) {

			String[] elements = {"--:--"};
			
			if(currentTimeEvent==null){return elements;}
			
			if(!TimeTrackerPlugin.getDefault().isCountingUp() && currentTimeEvent.getMinutes()==0 && currentTimeEvent.getSeconds()==0){
				elements[0]="Time out!";
				}
			else elements[0] = currentTimeEvent.getFormattedString();
						return elements;
			
			
		}

		
		
		
//	public void setElement(Object parent,String s)	
//	{
//	}
	
	}
	
	class ViewContentProvider1 implements IStructuredContentProvider {
		
		public ViewContentProvider1(){			
			System.out.println("Test");
		}
		
		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		}
		
		public void dispose() {
		}
		
		public Object[] getElements(Object parent) {
			content = log.readLog();
			String[] elements = {""};
			elements[0]=content;
		return elements;
		}
				
	}
	
	
	
	
	class ViewLabelProvider extends LabelProvider implements ITableLabelProvider {
		public String getColumnText(Object obj, int index) {
			return getText(obj);
		}
		public Image getColumnImage(Object obj, int index) {
			return getImage(obj);
		}
		public Image getImage(Object obj) {
			return PlatformUI.getWorkbench().
					getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT);
		}
	}

	/**
	 * The constructor.
	 */
	public TimeTrackerGUIViewer() {
		
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent)
	{
		
		//SashForm sash = 
		composite = new SashForm(parent, SWT.VERTICAL);
		//composite = parent;
		TimeTicker tt = new TimeTicker();
		log = new Logger(tt);
		tt.addObserver(this);
		//composite.setLayout(new FormLayout());
		//Composite buttons = new Group(composite, SWT.SHADOW_NONE);
		
		guiInteraction = new TimeTrackerGUIInteraction(composite, log);
		
		guiInteraction.addTimeTracker(tt); //TODO add correct TimeTracker reference here!!!
		
		tt.addObserver(guiInteraction);
		
		
		
		
	SashForm composite1 = new SashForm(parent, SWT.VERTICAL);
	
	TableViewer viewer1 = new TableViewer(composite1, SWT.MULTI);
	viewer1.setContentProvider(new ViewContentProvider1());
	viewer1.setLabelProvider(new ViewLabelProvider());
	viewer1.setInput(getViewSite());
	
				
		viewer = new TableViewer(composite, SWT.MULTI);
		viewer.setContentProvider(new ViewContentProvider());
		viewer.setLabelProvider(new ViewLabelProvider());
		
		for (int i = 0; i < composite.getChildren().length-1; i++) {
			composite.getChildren()[i].setFont(new Font(null, "Arial",60,1));
		}
		composite.getChildren()[4].setFont(new Font(null, "Arial",80,1));
		
		viewer.setInput(getViewSite());
		
		//viewer.add(tti.getComposite());
	}


	private void showMessage(String message) {
		MessageDialog.openInformation(
			viewer.getControl().getShell(),
			"Time Tracker",
			message);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		viewer.getControl().setFocus();
	}


	public void notify(TimeEvent time) {
		currentTimeEvent = time;
		if(!composite.isDisposed())
			TimeTrackerPlugin.getDefault().getWorkbench().getDisplay().syncExec(
					new Runnable(){
						public void run() {
								viewer.setInput(getViewSite());
						}
					});
	}
	
	/**
	 * @return Returns the viewer.
	 */
	TableViewer getViewer() {
		return viewer;
	}
	/**
	 * @return Returns the composite.
	 */
	Composite getComposite() {
		return composite;
	}
	/**
	 * @return Returns the guiInteraction.
	 */
	TimeTrackerGUIInteraction getGuiInteraction() {
		return guiInteraction;
	}
}
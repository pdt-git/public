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

package org.cs3.pdt.console.internal.views;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.pdt.runtime.PrologContextTracker;
import org.cs3.pdt.runtime.PrologContextTrackerListener;
import org.cs3.pdt.runtime.PrologContextTrackerService;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowPulldownDelegate2;

public abstract class SelectContextsAction extends Action implements IMenuCreator,
		IWorkbenchWindowPulldownDelegate2, PrologContextTrackerListener {

	private Set<String> activeTrackers;
	
	/**
	 * Cascading menu
	 */
	private Menu fCreatedMenu;

	/**
	 * Presentation wrapper for this action
	 */
	private IAction fAction;

	/**
	 * Creates a cascading menu action to populate with shortcuts in the given
	 * launch group.
	 * 
	 * @param launchGroupIdentifier
	 *            launch group identifier
	 */
	public SelectContextsAction() {
		super();

		setText(null);
		setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.TRACK_CONTEXT));		
		setMenuCreator(this);
	}

	
	
	/**
	 * @see IAction#run()
	 */
	public void run() {
		// do nothing, this action just creates a cascading menu.
	}

	public PrologInterface getCurrentPrologInterface(){
		
		for (Iterator<String> it = getActiveTrackers().iterator(); it.hasNext();) {
			String trackerId = it.next();
			PrologContextTracker tracker = PrologRuntimePlugin.getDefault().getContextTrackerService().getContextTracker(trackerId);
			if(tracker == null)
			{
				return null;
			}
			PrologInterface pif=null;
			pif = tracker.getCurrentPrologInterface();
			if(pif!=null){
				return pif;
			}
		}
		return null;
	}
	
	private void createAction(Menu parent, final PrologContextTracker tracker) {
		final String trackerId = tracker.getId();
		IAction action = new Action(tracker.getLabel(),IAction.AS_CHECK_BOX){
			public void run() {
				
				if(getActiveTrackers().contains(trackerId)){
					getActiveTrackers().remove(trackerId);
					tracker.removePrologContextTrackerListener(SelectContextsAction.this);
					trackerDeactivated(tracker);
				}else{
					getActiveTrackers().add(trackerId);
					tracker.addPrologContextTrackerListener(SelectContextsAction.this);
					trackerActivated(tracker);
				}
				PrologConsolePlugin.getDefault().setPreferenceValue(PDTConsole.PREF_CONTEXT_TRACKERS,Util.splice(getActiveTrackers(),","));
				setChecked(getActiveTrackers().contains(trackerId));
			}
		};
		action.setChecked(getActiveTrackers().contains(trackerId));
		ActionContributionItem item = new ActionContributionItem(action);
		
		item.fill(parent, -1);
	}

	protected abstract void trackerActivated(PrologContextTracker tracker) ;



	protected abstract void trackerDeactivated(PrologContextTracker tracker) ;



	/**
	 * @see IMenuCreator#dispose()
	 */
	public void dispose() {
		if (getCreatedMenu() != null) {
			getCreatedMenu().dispose();
		}
	}

	/**
	 * @see IMenuCreator#getMenu(Control)
	 */
	public Menu getMenu(Control parent) {
		if (getCreatedMenu() != null) {
			getCreatedMenu().dispose();
		}
		setCreatedMenu(new Menu(parent));
		fillMenu();
		initMenu();
		return getCreatedMenu();
	}

	/**
	 * @see IMenuCreator#getMenu(Menu)
	 */
	public Menu getMenu(Menu parent) {
		if (getCreatedMenu() != null) {
			getCreatedMenu().dispose();
		}
		setCreatedMenu(new Menu(parent));
		fillMenu();
		initMenu();
		return getCreatedMenu();
	}

	private void fillMenu() {
		PrologContextTracker[] trackers = PrologRuntimePlugin.getDefault().getContextTrackerService().getContextTrackers();
		Menu menu=getCreatedMenu();
		for (int i = 0; i < trackers.length; i++) {
			
			createAction(menu, trackers[i]);
		}
	}

	/**
	 * Creates the menu for the action
	 */
	private void initMenu() {
		// Add listener to repopulate the menu each time
		// it is shown to reflect changes in selection or active perspective
		fCreatedMenu.addMenuListener(new MenuAdapter() {
			public void menuShown(MenuEvent e) {
				Menu m = (Menu) e.widget;
				MenuItem[] items = m.getItems();
				for (int i = 0; i < items.length; i++) {
					items[i].dispose();
				}
				fillMenu();
			}
		});
	}

	

	private Menu getCreatedMenu() {
		return fCreatedMenu;
	}

	private void setCreatedMenu(Menu createdMenu) {
		fCreatedMenu = createdMenu;
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
	 */
	public void init(IWorkbenchWindow window) {
		// if (window instanceof WorkbenchWindow) {
		// fKeyBindingService= ((WorkbenchWindow)window).getKeyBindingService();
		// }
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		// do nothing - this is just a menu
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *      org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		if (fAction == null) {
			initialize(action);
		}
	}

	/**
	 * Set the enabled state of the underlying action based on whether there are
	 * any registered launch shortcuts for this launch mode.
	 */
	private void initialize(IAction action) {
		fAction = action;

	}



	



	Set<String> getActiveTrackers() {
		if(activeTrackers==null){
			activeTrackers=new HashSet<String>();			
			Util.split(PrologConsolePlugin.getDefault().getPreferenceValue(PDTConsole.PREF_CONTEXT_TRACKERS,""),",",activeTrackers);
			PrologContextTrackerService trackerService = PrologRuntimePlugin.getDefault().getContextTrackerService();
			for (Iterator<String> iter = activeTrackers.iterator(); iter.hasNext();) {
				String id = iter.next();				
				PrologContextTracker contextTracker = trackerService.getContextTracker(id);
				if(contextTracker!=null){
					contextTracker.addPrologContextTrackerListener(this);
				}
			}
		}
		return activeTrackers;
	}

}

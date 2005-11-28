package org.cs3.pdt.console.internal.views;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.pdt.runtime.PrologContextTracker;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologInterfaceRegistryEvent;
import org.cs3.pdt.runtime.PrologInterfaceRegistryListener;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowPulldownDelegate2;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.internal.ide.dialogs.UpdateProjectCapabilityWizard;

public abstract  class SelectPifAction extends Action implements IMenuCreator,
		IWorkbenchWindowPulldownDelegate2{

	/**
	 * Cascading menu
	 */
	private Menu fCreatedMenu;

	/**
	 * Presentation wrapper for this action
	 */
	private IAction fAction;

	private IWorkbenchWindow window;

	/**
	 * Creates a cascading menu action to populate with shortcuts in the given
	 * launch group.
	 * 
	 * @param launchGroupIdentifier
	 *            launch group identifier
	 */
	public SelectPifAction() {
		super();
		
		setText(null);
		setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.SELECT_PIF));
		setMenuCreator(this);
		
		
		
	}

	/**
	 * @see IAction#run()
	 */
	public void run() {
		// do nothing, this action just creates a cascading menu.
	}

	

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
		PrologInterfaceRegistry reg = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		Set keys = reg.getAllKeys();
		Menu menu=getCreatedMenu();
		for (Iterator it = keys.iterator(); it.hasNext();) {
			String key = (String) it.next();
			createAction(menu, reg, key);
		}	
			
		
	}
	public void update(){
		if (window==null){
			return;
		}
		Display display = window.getShell().getDisplay();
		if(display!=Display.getCurrent()){
			display.asyncExec(new Runnable() {
			
				public void run() {
					update();
			
				}
			
			});
			return;
		}
		PrologInterfaceRegistry reg = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		PrologInterface pif = getPrologInterface();
		if (pif==null){
			setToolTipText("no pif selected");
			return;
		}
		String key = reg.getKey(pif);
		if(key==null){
			setToolTipText("unregisterd Prologinterface???");
			return;
		}
		setToolTipText(key);
		
	}
	private void createAction(Menu menu, final PrologInterfaceRegistry reg, final String key) {
		IAction action = new Action(key,IAction.AS_RADIO_BUTTON){
			public void run() {
				setPrologInterface(PrologRuntimePlugin.getDefault().getPrologInterface(key));
			}

			
		};
		action.setChecked(key.equals(reg.getKey(getPrologInterface())));
		StringBuffer buf = new StringBuffer();
		Set subs = reg.getSubscriptionsForPif(key);
		buf.append(key);
		buf.append(": ");
		for (Iterator it = subs.iterator(); it.hasNext();) {
			Subscription sub = (Subscription) it.next();
			buf.append(sub.getName());
			if(it.hasNext()){
				buf.append(", ");
			}
		}
		//action.setToolTipText(buf.toString());
		action.setText(buf.toString());
		ActionContributionItem item = new ActionContributionItem(action);
		
		item.fill(menu, -1);
		//menu.getItem(menu.getItemCount()-1).
	}
	protected abstract void setPrologInterface(PrologInterface prologInterface) ;
	protected abstract PrologInterface getPrologInterface() ;

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
		this.window=window;
		
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

	public void prologInterfaceAdded(PrologInterfaceRegistryEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void prologInterfaceRemoved(PrologInterfaceRegistryEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void subscriptionAdded(PrologInterfaceRegistryEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void subscriptionRemoved(PrologInterfaceRegistryEvent e) {
		// TODO Auto-generated method stub
		
	}

}

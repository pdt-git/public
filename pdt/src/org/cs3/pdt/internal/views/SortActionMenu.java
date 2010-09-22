/**
 * 
 */
package org.cs3.pdt.internal.views;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.common.Util;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowPulldownDelegate2;

final class SortActionMenu extends Action implements IMenuCreator,
		IWorkbenchWindowPulldownDelegate2 {
	private Menu fCreatedMenu;

	

	private Object fAction;

	private PrologOutline outline;

	SortActionMenu(PrologOutline outline) {
		super("Sort", ImageRepository
				.getImageDescriptor(ImageRepository.SORT));
		this.outline=outline;
		setMenuCreator(this);
		
	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	private Menu getCreatedMenu() {
		return fCreatedMenu;
	}

	private void setCreatedMenu(Menu createdMenu) {
		fCreatedMenu = createdMenu;
	}

	/**
	 * @see IMenuCreator#getMenu(Control)
	 */
	@Override
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
	@Override
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

		Menu menu = getCreatedMenu();
		PrologOutlineFilter[] filters = outline.getAvailableFilters();
		for (int i = 0; i < filters.length; i++) {
			createAction(menu,filters[i]);
		}
		

	}

	private void createAction(Menu parent, final PrologOutlineFilter filter) {

		IAction action = new Action(filter.getLabel(), IAction.AS_CHECK_BOX) {
			@Override
			public void run() {
				if (isFilterActive(filter)) {
					getViewer().removeFilter(filter);
				} else {
					getViewer().addFilter(filter);
				}
				setChecked(isFilterActive(filter));
				PDTPlugin.getDefault().setPreferenceValue(PDT.PREF_OUTLINE_FILTERS,Util.splice(getViewer().getFilters(),","));

			}
			
		};
		action.setChecked(isFilterActive(filter));
		ActionContributionItem item = new ActionContributionItem(action);

		item.fill(parent, -1);
	}

	private boolean isFilterActive(ViewerFilter filter) {
		for (int i = 0; i < getViewer().getFilters().length; i++) {
			ViewerFilter elm = getViewer().getFilters()[i];
			if (filter == elm) {
				return true;
			}
		}
		return false;
	}

	private TreeViewer getViewer() {
		return outline.getTreeViewer();
	}

	/**
	 * Creates the menu for the action
	 */
	private void initMenu() {
		// Add listener to repopulate the menu each time
		// it is shown to reflect changes in selection or active perspective
		fCreatedMenu.addMenuListener(new MenuAdapter() {
			@Override
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

	@Override
	public void init(IWorkbenchWindow window) {
		// TODO Auto-generated method stub

	}

	@Override
	public void run(IAction action) {
		// TODO Auto-generated method stub

	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *      org.eclipse.jface.viewers.ISelection)
	 */
	@Override
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
}
/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Ilshat Aliev
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package pdt.y.focusview;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;

import pdt.y.internal.ImageRepository;
import pdt.y.main.PDTGraphView;


public class LogtalkView extends ViewBase {
	
	private static final String VIEW_NAME = "Logtalk Graph";

	public enum InputType {
		PROJECT(ImageRepository.INPUT_TYPE_PROJECT, "Project"),
		LIBRARY(ImageRepository.INPUT_TYPE_LIBRARY, "Library"),
		RECURSIVE_LIBRARY(ImageRepository.INPUT_TYPE_RECURSIVE_LIBRARY, "Recursive Library");
		
		private final String icon;
		private final String label;
		
		public Image getImage() {
			return ImageRepository.getImage(icon);
		}
		
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(icon);
		}
		
		public String getLabel() {
			return label;
		}
		
		private InputType(String icon, String label){
			this.icon = icon;
			this.label = label;
		}
	}
	
	public enum DiagramType {
		ENTITY("entity_diagram", "Entity diagram"),
		CALL("xref_diagram", "Cross reference diagram"),
		INHERITANCE("inheritance_diagram", "Inheritance diagram"),
		USES("uses_diagram", "Uses diagram"),
		FILE_LOAD("file_load_diagram", "File load diagram"),
		FILE_DEPENDS("file_dependency_diagram", "File dependency diagram");
		
		public String getDiagramEntity() {
			return diagramEntity;
		}
		
		public String getLabel() {
			return label;
		}
		
		private final String diagramEntity;
		private final String label;

		private DiagramType(String diagramEntity, String label) {
			this.diagramEntity = diagramEntity;
			this.label = label;
		}
	}
	
	private DiagramType diagramType = DiagramType.ENTITY;
	
	private LogtalkViewCoordinator logtalkViewCoordinator;
	private String statusText = "";
	
	private InputType inputType = InputType.PROJECT;
	
	private String currentLibrary;
	private Action inputSelector;
	
	@Override
	public GraphPIFLoaderBase createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new LogtalkGraphPIFLoader(pdtGraphView, this);
	}

	@Override
	protected ViewCoordinatorBase createViewCoordinator() {
		logtalkViewCoordinator = new LogtalkViewCoordinator(this);
		return logtalkViewCoordinator;
	}
	
	@Override
	protected void initButtons(Composite parent) {
		IActionBars actionBars = getViewSite().getActionBars();
		
		IToolBarManager toolBarManager = actionBars.getToolBarManager();
		inputSelector = new Action("Select input type", IAction.AS_DROP_DOWN_MENU) {
			@Override
			public void run() {
				if (getInputType() != InputType.PROJECT) {
					String result = askForLibrary(getInputType());
					if (result != null) {
						setCurrentLibrary(result);
						logtalkViewCoordinator.diagramSettingsChanged();
					}
				}
			}
		};
		inputSelector.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.INPUT_TYPE_PROJECT));
		inputSelector.setMenuCreator(new IMenuCreator() {
			private Menu menu;
			
			@Override
			public Menu getMenu(Menu parent) {
				return null;
			}
			
			@Override
			public Menu getMenu(Control parent) {
				Menu menu = new Menu(parent);
				createMenuItem(menu, InputType.PROJECT);
				createMenuItem(menu, InputType.LIBRARY);
				createMenuItem(menu, InputType.RECURSIVE_LIBRARY);
				this.menu = menu;
				return menu;
			}
			
			private void createMenuItem(final Menu menu, final InputType inputType) {
				final MenuItem item = new MenuItem(menu, SWT.RADIO);
				item.setText(inputType.getLabel());
				item.setImage(inputType.getImage());
				item.setSelection(getInputType() == inputType);
				item.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (!item.getSelection()) {
							return;
						}
						if (inputType != InputType.PROJECT) {
							String result = askForLibrary(inputType);
							if (result != null) {
								setInputType(inputType);
								setCurrentLibrary(result);
							}
						} else {
							setInputType(inputType);
						}
					}
				});
			}
			
			@Override
			public void dispose() {
				if (menu != null) {
					menu.dispose();
					menu = null;
				}
			}
		});
		toolBarManager.add(inputSelector);
		
		Action typeSelector = new Action("Select diagram type", IAction.AS_DROP_DOWN_MENU) {
		};
		typeSelector.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CONFIGURE_LOGTALK_GRAPH));
		typeSelector.setMenuCreator(new IMenuCreator() {
			private Menu menu;
			
			@Override
			public Menu getMenu(Menu parent) {
				return null;
			}
			
			@Override
			public Menu getMenu(Control parent) {
				Menu menu = new Menu(parent);
				createMenuItem(menu, DiagramType.ENTITY);
				createMenuItem(menu, DiagramType.CALL);
				createMenuItem(menu, DiagramType.INHERITANCE);
				createMenuItem(menu, DiagramType.USES);
				createMenuItem(menu, DiagramType.FILE_LOAD);
				createMenuItem(menu, DiagramType.FILE_DEPENDS);
				this.menu = menu;
				return menu;
			}
			
			private void createMenuItem(final Menu menu, final DiagramType diagramType) {
				final MenuItem item = new MenuItem(menu, SWT.RADIO);
				item.setText(diagramType.getLabel());
				item.setSelection(getDiagramType() == diagramType);
				item.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (!item.getSelection()) {
							return;
						}
						setDiagramType(diagramType);
					}
				});
			}
			
			@Override
			public void dispose() {
				if (menu != null) {
					menu.dispose();
					menu = null;
				}
			}
		});
		toolBarManager.add(typeSelector);

		super.initButtons(parent);
	}
	
	protected String askForLibrary(InputType inputType2) {
		String[] logtalkLibraries = getLogtalkLibraries();
		if (logtalkLibraries == null) {
			MessageDialog.openWarning(getSite().getShell(), VIEW_NAME, "Could not determine Logtalk libraries!");
			return null;
		}
		if (logtalkLibraries.length == 0) {
			MessageDialog.openWarning(getSite().getShell(), VIEW_NAME, "No Logtalk library found!");
			return null;
		}
		ElementListSelectionDialog dialog = new ElementListSelectionDialog(getSite().getShell(), new LabelProvider());
		dialog.setBlockOnOpen(true);
		dialog.setTitle(VIEW_NAME);
		dialog.setMessage("Select " + inputType2.getLabel());
		dialog.setMultipleSelection(false);
		dialog.setElements(logtalkLibraries);
		dialog.setHelpAvailable(false);
		dialog.open();
		Object[] result = dialog.getResult();
		if (result != null && result.length == 1) {
			return (String) result[0];
		}
		return null;
	}

	public InputType getInputType() {
		return inputType;
	}

	private void setInputType(InputType inputType) {
		this.inputType = inputType;
		inputSelector.setImageDescriptor(inputType.getImageDescriptor());
		updateStatusText();
		logtalkViewCoordinator.diagramSettingsChanged();
	}

	public String getCurrentLibrary() {
		return currentLibrary;
	}
	
	private void setCurrentLibrary(String currentLibrary) {
		this.currentLibrary = currentLibrary;
	}
	
	public DiagramType getDiagramType() {
		return diagramType;
	}
	
	private void setDiagramType(DiagramType diagramType) {
		this.diagramType = diagramType;
		updateStatusText();
		logtalkViewCoordinator.diagramSettingsChanged();
	}
	
	@Override
	public void setStatusText(String text) {
		this.statusText = text;
		super.setStatusText(constructStatusText(text));
	}

	private void updateStatusText() {
		super.setStatusText(constructStatusText(statusText));
	}
	
	private String constructStatusText(String suffix) {
		StringBuilder buf = new StringBuilder(diagramType.getLabel());
		buf.append(", ");
		buf.append(inputType.getLabel());
		if (inputType != InputType.PROJECT) {
			buf.append(": ");
			buf.append(currentLibrary);
		}
		buf.append("     ");
		buf.append(suffix);
		return buf.toString();
	}
	
	private String[] getLogtalkLibraries() {
		ArrayList<String> libraries = new ArrayList<String>();
		PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();
		List<Map<String, Object>> results;
		try {
			results = pif.queryAll(
					bT("current_predicate", "logtalk_load/1"),
					bT("logtalk_library_path", "Library", "_"));
			for (Map<String, Object> result : results) {
				libraries.add((String) result.get("Library"));
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
			return null;
		}
		return libraries.toArray(new String[libraries.size()]);
	}
	
}

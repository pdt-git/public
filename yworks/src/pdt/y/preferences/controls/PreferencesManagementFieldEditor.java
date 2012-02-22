package pdt.y.preferences.controls;

import static pdt.y.preferences.PreferenceConstants.BASE_TEMPLATE;
import static pdt.y.preferences.PreferenceConstants.BASE_TEMPLATE_DEFAULT;

import java.io.FileNotFoundException;
import java.util.Hashtable;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.progress.UIJob;

import pdt.y.preferences.PreferenceInitializer;

public class PreferencesManagementFieldEditor extends FieldEditor {

	private Combo templatesCombo;
	private Text newTemplateName;
	
	public PreferencesManagementFieldEditor(Composite parent) {
		
		init(BASE_TEMPLATE, "Preferences Management");
		createControl(parent);
    }
	
	@Override
	protected void adjustForNumColumns(int numColumns) { }

	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		
		Composite container = new Composite(parent, SWT.NONE);
		
		GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 20;
        layout.horizontalSpacing = HORIZONTAL_GAP;
        layout.numColumns = 4;
        
        container.setLayout(layout);
		
		GridData fullRowgd = new GridData();
		fullRowgd.horizontalSpan = 4;
		getLabelControl(container).setLayoutData(fullRowgd);
		
        GridData gd = new GridData();
        gd.widthHint = 120;
        
        GridData gdBtns = new GridData();
        gdBtns.widthHint = 70;
        
		new Label(container, SWT.NONE).setText("Import base template:");
		templatesCombo = new Combo(container, SWT.DROP_DOWN | SWT.READ_ONLY);
		templatesCombo.setLayoutData(gd);
        createImportButton(container).setLayoutData(gdBtns);
        createRemoveAllTemplatesButton(container).setLayoutData(gdBtns);
        
        new Label(container, SWT.NONE).setText("Save current preferences as template:");
        newTemplateName = new Text(container, SWT.BORDER);
        newTemplateName.setLayoutData(gd);
        createSavePreferencesSetButton(container).setLayoutData(gdBtns);
        
        Composite importExportButtonsContainer = new Composite(container, SWT.NONE);
        importExportButtonsContainer.setLayoutData(fullRowgd);
        
        GridLayout bottomLayout = new GridLayout();
        bottomLayout.horizontalSpacing = HORIZONTAL_GAP;
        bottomLayout.numColumns = 2;

        importExportButtonsContainer.setLayout(bottomLayout);
        
        createSaveCurrentTemplateToFileButton(importExportButtonsContainer);
        createLoadTemplateFromFileButton(importExportButtonsContainer);
	}

	private Button createRemoveAllTemplatesButton(Composite container) {
		Button button = new Button(container, SWT.PUSH);
		
		button.setText(" Clear list ");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				MessageBox mb = new MessageBox(getPage().getShell(), SWT.APPLICATION_MODAL | SWT.OK | SWT.CANCEL);
				mb.setText("WARNING");
				mb.setMessage("All base templates except default will be removed!");
				if (mb.open() == SWT.OK) {
					PreferenceInitializer.removeAllTemplates(getPreferenceStore());
					updateUI();
				}
			}
		});

		return button;
	}

	private void createLoadTemplateFromFileButton(Composite container) {
		Button button = new Button(container, SWT.PUSH);
		
		button.setText(" Load template from file ");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog fd = new FileDialog(getPage().getShell(), SWT.OPEN);
		        fd.setText("Open");
		        String path = fd.open();
		        if (path != null) {
		        	try {
						PreferenceInitializer.loadPreferencesFromFile(getPreferenceStore(), path);
						clearErrorMessage();
						showInfo("Template loaded and applied");
					} catch (Exception ex) {
						showMessage(ex.getMessage());
					}
		        }
			}
		});
	}

	private void createSaveCurrentTemplateToFileButton(Composite container) {
		Button button = new Button(container ,SWT.PUSH);
		
		button.setText(" Save template to file ");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog fd = new FileDialog(getPage().getShell(), SWT.SAVE);
		        fd.setText("Save");
		        String path = fd.open();
		        if (path != null) {
		        	try {
						PreferenceInitializer.saveCurrentPreferencesToFile(getPreferenceStore(), path);
						clearErrorMessage();
						showInfo("Template saved");
					} catch (FileNotFoundException ex) {
						showErrorMessage(ex.getMessage());
					}
	        	}
			}
		});
	}

	protected Button createImportButton(Composite container) {
		Button button = new Button(container, SWT.PUSH);
		
		button.setText("Apply");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				MessageBox mb = new MessageBox(getPage().getShell(), SWT.APPLICATION_MODAL | SWT.OK | SWT.CANCEL);
				mb.setText("WARNING");
				mb.setMessage("Current focusview preferences will be overriden!");
				if (mb.open() == SWT.OK) {
					PreferenceInitializer.applyTemplate(getPreferenceStore(), templatesCombo.getItem(templatesCombo.getSelectionIndex()));
					updateUI();
				}
			}
		});
		
		return button;
	}
	
	private Button createSavePreferencesSetButton(Composite container) {
		Button button = new Button(container ,SWT.PUSH);
		
		button.setText("Save");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					getPreferencePage().performOk();
					PreferenceInitializer.saveCurrentTemplate(getPreferenceStore(), newTemplateName.getText());
					clearErrorMessage();
					updateUI();
					clearErrorMessage();
				}
				catch (IllegalArgumentException ex) {
					showErrorMessage(ex.getMessage());
				}
			}
		});
		return button;
	}
	
	protected void updateUI() {
		new UIJob("Updating...") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				newTemplateName.setText("");
				loadItems();
				return Status.OK_STATUS;
			}
		
		}.schedule();
	}

	@Override
	protected void doLoad() {
		loadItems();	
	}
	
	@Override
	protected void doLoadDefault() {
		loadItems();
	}

	private void loadItems() {
		if (templatesCombo == null)
			return;
		
		templatesCombo.removeAll();
		
		templatesCombo.add(BASE_TEMPLATE_DEFAULT);
		templatesCombo.select(0);
		
		String val = getPreferenceStore().getString(BASE_TEMPLATE);

		Hashtable<String,String[][]> storedPrefs = PreferenceInitializer.getTemplates(getPreferenceStore());
		if (storedPrefs != null) {
			Set<String> keys = storedPrefs.keySet();
			
			int i = 1;
			for (String k : keys) {
				templatesCombo.add(k);
				
				if (k.equals(val)) {
					templatesCombo.select(i);
				}
				i++;
			}
		}
	}

	@Override
	protected void doStore() { }

	@Override
	public int getNumberOfControls() {
		return 1;
	}
	
	private void showInfo(String string) {
		MessageBox mb = new MessageBox(getPage().getShell(), SWT.APPLICATION_MODAL | SWT.OK);
		mb.setText("Info");
		mb.setMessage(string);
		mb.open();
	}
}

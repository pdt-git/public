package org.cs3.pdt.transform.internal.wizards;

import java.util.HashMap;

import org.cs3.pdt.transform.internal.PrologRefactoringInfo;
import org.cs3.pdt.ui.util.OptionEditor;
import org.cs3.pdt.ui.util.PropertyEditor;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.OptionProvider;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ltk.ui.refactoring.UserInputWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

public class PrologRefactoringInputPage extends UserInputWizardPage {

	private PrologRefactoringInfo info;
	private HashMap<String, PropertyEditor> editors = new HashMap<String, PropertyEditor>();

	public PrologRefactoringInputPage(PrologRefactoringInfo info) {
		super(info.getName());
		this.info = info;
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		setControl(composite);
		GridLayout layout = new GridLayout();
		composite.setLayout(layout);
		GridData data = new GridData(GridData.FILL);
		data.grabExcessHorizontalSpace = true;
		data.verticalAlignment = GridData.FILL;
		data.horizontalAlignment = GridData.FILL;
		composite.setLayoutData(data);

		OptionProvider jtransformerProject = info;
		Option[] options = jtransformerProject.getOptions();
		addEditorsForOptions(composite, options);
		load();

	}

	private void addEditorsForOptions(Composite parent, Option[] options) {

		for (int i = 0; i < options.length; i++) {
			if (options[i].isVisible()) {
				final PropertyEditor editor = OptionEditor.create(parent,
						options[i]);
				// disable the editor, if the value is overridden per sys prop.
				editor.setEnabled(System.getProperty(editor.getKey()) == null);
				editors.put(editor.getKey(), editor);
				editor.addPropertyChangeListener(new IPropertyChangeListener() {
					@Override
					public void propertyChange(PropertyChangeEvent e) {
						info.setPreferenceValue(editor.getKey(), editor
								.getValue());
					}

				});
				editor.getControl().setToolTipText(options[i].getDescription());
				editor.setEnabled(options[i].isEditable());
			}
		}
	}

	private void load() {
		Option[] options = info.getOptions();
		for (int i = 0; i < options.length; i++) {
			Option option = options[i];
			String id = option.getId();
			PropertyEditor editor = editors.get(id);

			if (options[i].isVisible()) {
				try {
					String value = info.getPreferenceValue(id, "");
					editor.setValue(value);
				} catch (Throwable e) {
					Debug.report(e);
					setErrorMessage("ERROR: could not read property " + id
							+ "\nSee log for details.");

				}
			}
		}

	}

}

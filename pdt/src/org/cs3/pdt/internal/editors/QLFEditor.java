
package org.cs3.pdt.internal.editors;

import org.cs3.pdt.internal.actions.ConsultAction;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.part.MultiPageEditorPart;

public class QLFEditor extends MultiPageEditorPart {

	@Override
	protected void createPages() {
		Composite composite = new Composite(getContainer(), SWT.NONE);
		GridLayout layout = new GridLayout();
		composite.setLayout(layout);
		layout.numColumns = 2;

		Button consultButton = new Button(composite, SWT.NONE);
		GridData gd = new GridData(GridData.BEGINNING);
		gd.horizontalSpan = 2;
		consultButton.setLayoutData(gd);
		consultButton.setText("Consult");
		consultButton.addListener(SWT.Selection, new Listener() {

			@Override
			public void handleEvent(Event event) {

				new ConsultAction().consultFromActiveEditor();
			}
			
		});
		setPartName(getEditorInput().getName());

		addPage(composite);
	}

	@Override
	public void doSave(IProgressMonitor monitor) {}

	@Override
	public void doSaveAs() {}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

	
}
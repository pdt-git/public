package org.cs3.prolog.ui.util.preferences;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class MyLabelFieldEditor extends FieldEditor implements FieldEditorForStructuredPreferencePage {
	
	private Label label2;
	private Composite parent;
	
	public MyLabelFieldEditor(Composite parent, String name) {
		super("", name, parent);
		this.parent = parent;
	}

	@Override
	protected void adjustForNumColumns(int numColumns) {
        GridData gd = (GridData) label2.getLayoutData();
        gd.horizontalSpan = numColumns - 1;
        gd.grabExcessHorizontalSpace = (gd.horizontalSpan == 1);
	}

	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
        getLabelControl(parent);
        label2 = new Label(parent, SWT.LEFT | SWT.WRAP);
        label2.setFont(parent.getFont());
        String text = getLabelText();
        if (text != null) {
			label2.setText(text);
		}
        label2.addDisposeListener(new DisposeListener() {
            @Override
			public void widgetDisposed(DisposeEvent event) {
                label2 = null;
            }
        });
        GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = true;
        label2.setLayoutData(gd);
	}

	@Override
	protected void doLoad() {
	}

	@Override
	protected void doLoadDefault() {
	}

	@Override
	protected void doStore() {
	}

	@Override
	public int getNumberOfControls() {
		return 2;
	}

	@Override
	public void adjustColumns(int numColumns) {
		adjustForNumColumns(numColumns);
	}

	@Override
	public Composite getParent() {
		return parent;
	}
	
	public void setText(String text) {
		label2.setText(text);
	}

}

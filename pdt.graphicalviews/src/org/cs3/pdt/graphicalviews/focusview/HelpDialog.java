package org.cs3.pdt.graphicalviews.focusview;

import org.cs3.pdt.graphicalviews.internal.ImageRepository;
import org.eclipse.jface.window.ApplicationWindow;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

public class HelpDialog extends ApplicationWindow {
	
	public HelpDialog(Shell parentShell) {
		super(parentShell);
	}

	@Override
	protected void configureShell(Shell shell) {
		super.configureShell(shell);

		// Set the title bar text
		shell.setText("PDT Graphical Views Help");
	}
	
	@Override
	protected Control createContents(Composite parent) {

		Composite compi = new Composite(parent, SWT.NONE);
		compi.setLayout(new GridLayout(1,false));

		Composite masterComposite = new Composite(compi , SWT.NONE);
		masterComposite.setLayout(new GridLayout(1, false));
		/*
		 * Icon Legend
		 */
		Group nodeGroup = createGroup(masterComposite, "Predicates", 4);

		addLegendItem(nodeGroup, "default.png", "Normal");
		addLegendItem(nodeGroup, "pred_dynamic.png", "Dynamic");
		addLegendItem(nodeGroup, "pred_declared_meta.png", "Declared meta");
		addLegendItem(nodeGroup, "pred_inferred_meta.png", "Inferred meta");
		
		Group callGroup = createGroup(masterComposite, "Calls", 2);
		addLegendItem(callGroup, "call_normal.png", "Normal call");
		addLegendItem(callGroup, "call_meta.png", "Metacall");
		addLegendItem(callGroup, "call_db.png", "assert/retract");

		Group visibilityGroup = createGroup(masterComposite, "Visibility", 2);
		addLegendItem(visibilityGroup, "default.png", "Exported Predicate");
		addLegendItem(visibilityGroup, "visibility_local.png", "Local Predicate");
		addLegendItem(visibilityGroup, "visibility_local_uncalled.png", "Local Predicate, not called by any exported predicate");
		
		Button close = new Button(masterComposite, SWT.PUSH);
		close.setText("Close");
		close.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				close();
			}
		});

		return compi;
	}
	
	private Group createGroup(Composite masterComposite, String title, int cols) {
		Group group = new Group(masterComposite,SWT.SHADOW_ETCHED_IN);
		GridLayout layout = new GridLayout(cols, false);
		GridData data = new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL);
		group.setLayout(layout);
		group.setLayoutData(data);
		group.setText(title);
		return group;
	}

	private void addLegendItem(Group legendGroup, String iconName, String label) {
		Label iconLabel = new Label(legendGroup, SWT.NONE);
		iconLabel.setImage(ImageRepository.getImageDescriptor("help\\" + iconName).createImage());
		Label textLabel = new Label(legendGroup, SWT.NONE);
		textLabel.setText(label);
	}
}

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

		Composite masterComposite = new Composite(parent, SWT.NONE);
		masterComposite.setLayout(new GridLayout(1,true));
		
//		Composite masterComposite = new Composite(compi , SWT.NONE);
//		masterComposite.setLayout(new GridLayout(1, false));
		/*
		 * Icon Legend
		 */
		
		Group predicateGroup = createGroup(masterComposite, "Predicates", 3);
		addLegendIcon(predicateGroup, "pred_normal.png");
		addLegendIcon(predicateGroup, "pred_meta.png");
		addLegendIcon(predicateGroup, "pred_dynamic.png");
		addLegendLabel(predicateGroup, "Normal predicate");
		addLegendLabel(predicateGroup, "Meta predicate");
		addLegendLabel(predicateGroup, "Dynamic predicate");
		
		Group callGroup = createGroup(masterComposite, "Calls", 3);
		addLegendIcon(callGroup, "call_normal.png");
		addLegendIcon(callGroup, "call_meta.png");
		addLegendIcon(callGroup, "call_dynamic.png");
		addLegendLabel(callGroup, "Normal call");
		addLegendLabel(callGroup, "Metacall");
		addLegendLabel(callGroup, "Dynamic call\n(assert/retract)");

//		Group visibilityGroup = createGroup(masterComposite, "Visibility", 2);
//		addLegendIcon(visibilityGroup, "visibility_exported.png");
//		addLegendIcon(visibilityGroup, "visibility_local.png");
//		addLegendLabel(visibilityGroup, "Exported Predicate");
//		addLegendLabel(visibilityGroup, "Local Predicate");
//
//		Group deadCodeGroup = createGroup(masterComposite, "Dead code detection", 2);
//		addLegendIcon(deadCodeGroup, "pred_normal.png");
//		addLegendIcon(deadCodeGroup, "pred_dead.png");
//		addLegendLabel(deadCodeGroup, "Not dead\n(called and/or exported)");
//		addLegendLabel(deadCodeGroup, "Dead\n(Uncalled and not exported)");
		
		
		
		// alternative
		Group visibilityGroup = createGroup(masterComposite, "Visibility and Dead code", 3);
		addLegendIcon(visibilityGroup, "pred_normal_exported.png");
		addLegendIcon(visibilityGroup, "pred_normal_local.png");
		addLegendIcon(visibilityGroup, "pred_dead.png");
		addLegendLabel(visibilityGroup, "Exported Predicate");
		addLegendLabel(visibilityGroup, "Local Predicate\n(not exported)");
		addLegendLabel(visibilityGroup, "Local and uncalled predicate\n(Dead code)");

		
		Button close = new Button(masterComposite, SWT.PUSH);
		close.setText("Close");
		close.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				close();
			}
		});

		return masterComposite;
	}
	
	private Group createGroup(Composite masterComposite, String title, int cols) {
		Group group = new Group(masterComposite,SWT.NONE);
		GridLayout layout = new GridLayout(cols, true);
		GridData data = new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL | GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_FILL);
		group.setLayout(layout);
		group.setLayoutData(data);
		group.setText(title);
		return group;
	}

	private void addLegendIcon(Group legendGroup, String iconName) {
		Label iconLabel = new Label(legendGroup, SWT.NONE);
		iconLabel.setImage(ImageRepository.getImageDescriptor("help\\" + iconName).createImage());
		GridData data = new GridData(GridData.CENTER, GridData.CENTER, true, true);
		iconLabel.setLayoutData(data);
	}
	
	private void addLegendLabel(Group legendGroup, String label) {
		Label textLabel = new Label(legendGroup, SWT.NONE);
		textLabel.setText(label);
		textLabel.setAlignment(SWT.CENTER);
		GridData data = new GridData(GridData.CENTER, GridData.BEGINNING, true, true);
		textLabel.setLayoutData(data);
	}
	
	private void addLegendItem(Group legendGroup, String iconName, String label) {
		Label iconLabel = new Label(legendGroup, SWT.NONE);
		iconLabel.setImage(ImageRepository.getImageDescriptor("help\\" + iconName).createImage());
		Label textLabel = new Label(legendGroup, SWT.NONE);
		textLabel.setText(label);
	}
}

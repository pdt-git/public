package pdt.y.preferences.controls;

import static pdt.y.preferences.PreferenceConstants.NODE_SIZE;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_FIXED;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_FIXED_HEIGHT;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_FIXED_WIDTH;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_INDIVIDUAL;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_MAXIMUM;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_MEDIAN;

import java.util.Hashtable;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class NodeSizeRadioGroupFieldEditor extends FieldEditor {
	
	String value;
	Hashtable<String, String> sizeValue = new Hashtable<String, String>();
	
	Button[] radioButtons;
	Text[] sizeTextboxes;
	
	public NodeSizeRadioGroupFieldEditor(Composite parent) {
		
		init(NODE_SIZE, "Predicate Node Size");
		createControl(parent);
    }
	
	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = getLabelControl(parent);
        GridData gd = new GridData();
        gd.horizontalSpan = numColumns;
        gd.verticalIndent = HORIZONTAL_GAP;
        control.setLayoutData(gd);
		
		control = getRadioBoxControl(parent);
        gd = new GridData();
        gd.horizontalSpan = numColumns;
        gd.horizontalIndent = HORIZONTAL_GAP;
        control.setLayoutData(gd);
    }

	protected Composite getRadioBoxControl(Composite parent) {
		
		Composite radioBox = new Composite(parent, SWT.NONE);
		
		GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.horizontalSpacing = HORIZONTAL_GAP;
        layout.numColumns = 1;
        radioBox.setLayout(layout);
		
        radioButtons = new Button[4];
        
		radioButtons[0] = createRadioButton(radioBox, "&Fixed", NODE_SIZE_FIXED);
		
		Composite pnlSize = new Composite(radioBox, SWT.NONE);
		layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.marginLeft = 50;
        layout.horizontalSpacing = HORIZONTAL_GAP;
        layout.numColumns = 4;
        pnlSize.setLayout(layout);
		
		
        sizeTextboxes = new Text[2];
		sizeTextboxes[0] = createTextBox(pnlSize, "Width", NODE_SIZE_FIXED_WIDTH, radioButtons[0]);
		sizeTextboxes[1] = createTextBox(pnlSize, "Height", NODE_SIZE_FIXED_HEIGHT, radioButtons[0]);
		
		radioButtons[1] = createRadioButton(radioBox, "Me&dian", NODE_SIZE_MEDIAN);
		radioButtons[2] = createRadioButton(radioBox, "Ma&ximum", NODE_SIZE_MAXIMUM);
		radioButtons[3] = createRadioButton(radioBox, "&Individual", NODE_SIZE_INDIVIDUAL);
		
		return radioBox;
	}

	public Button createRadioButton(Composite parent, final String text, final String data) {
		Button radio = new Button(parent, SWT.RADIO | SWT.LEFT);
        radio.setText(text);
        radio.setData(data);
        radio.setFont(parent.getFont());
        radio.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                String oldValue = value;
                value = (String) event.widget.getData();
                setPresentsDefaultValue(false);
                fireValueChanged(NODE_SIZE, oldValue, value);
            }
        });
        
		return radio;
	}
	
	private Text createTextBox(final Composite parent, final String text, final String name, final Button bindedButton) {
		Label l = new Label(parent, SWT.NONE);
		l.setText(text);
		
		final Text txt = new Text(parent, SWT.BORDER);
		
		Point s = txt.getSize();
		s.y = 50;
		txt.setSize(s);
		
		txt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				String oldValue = sizeValue.get(name);
				String newValue = txt.getText();
				
				sizeValue.put(name, newValue);
				
				checkValues();
				
                setPresentsDefaultValue(false);
                fireValueChanged(name, oldValue, newValue);
			}
		});
		
		bindedButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                txt.setEnabled(bindedButton.getSelection());
            }
        });
		
		return txt;
	}
	
	@Override
	public boolean isValid() {
		return checkValues();
	}
	
	private boolean checkValues() {
		for (String k : sizeValue.keySet()) {
			String v = sizeValue.get(k);
			try {
				Integer.parseInt(v);
			}
			catch (NumberFormatException ex) {
				showErrorMessage("Wrong number format");
				return false;
			}
		}
		
		clearErrorMessage();
		return true;
	}
	
	@Override
	protected void doLoad() {
		IPreferenceStore prefs = getPreferenceStore();
		
		updateRadioGroupValue(prefs.getString(NODE_SIZE));
		updateSizeTextBoxes(
				prefs.getString(NODE_SIZE_FIXED_WIDTH), 
				prefs.getString(NODE_SIZE_FIXED_HEIGHT));
	}

	@Override
	protected void doLoadDefault() {
		IPreferenceStore prefs = getPreferenceStore();
		
		updateRadioGroupValue(getPreferenceStore().getString(NODE_SIZE));
		updateSizeTextBoxes(
				prefs.getDefaultString(NODE_SIZE_FIXED_WIDTH), 
				prefs.getDefaultString(NODE_SIZE_FIXED_HEIGHT));
	}
	
	@Override
	protected void doStore() {
		if (!isValid()) 
			return;
		
		IPreferenceStore prefs = getPreferenceStore();
		
		if (value == null) {
            prefs.setToDefault(NODE_SIZE);
        }
		else {
			prefs.setValue(NODE_SIZE, value);
		}
		
		for (String k : sizeValue.keySet()) {
			String v = sizeValue.get(k);
			if (v == null) {
				prefs.setToDefault(k);
			}
			else {
				
				prefs.setValue(k, v);
			}
		}
	}
	
	private void updateRadioGroupValue(String selectedValue) {
        value = selectedValue;
        if (radioButtons == null) {
			return;
		}

        if (this.value != null) {
            for (int i = 0; i < radioButtons.length; i++) {
                Button radio = radioButtons[i];
                radio.setSelection(value.equals(radio.getData()));
            }
        }
    }

	private void updateSizeTextBoxes(final String width, final String height) {
		if (sizeTextboxes == null) {
			return;
		}
		
		sizeTextboxes[0].setText(width);
		sizeTextboxes[1].setText(height);
		
		boolean enabled = radioButtons[0].getSelection();
		
		sizeTextboxes[0].setEnabled(enabled);
		sizeTextboxes[1].setEnabled(enabled);
	}

	@Override
	protected void adjustForNumColumns(int numColumns) { }

	@Override
	public int getNumberOfControls() {
		return 1;
	}
}
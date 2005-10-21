/*
 */
package org.cs3.pdt.ui.util;

import org.cs3.pl.common.Option;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 */
public class FileEditor extends OptionEditor implements PropertyEditor {
    private static final int TEXT_FIELD_WIDTH = 20;
    private Text valueField;
    private Button button;
    public FileEditor(Composite parent, Option option) {
        super(parent, option);

    }

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.OptionEditor#createControls(org.eclipse.swt.widgets.Composite)
     */
    protected void createControls(Composite composite) {
        GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        composite.setLayout(layout);
        
        GridData data = new GridData(GridData.FILL);
        data.grabExcessHorizontalSpace = true;
        data.verticalAlignment = GridData.FILL;
        data.horizontalAlignment = GridData.FILL;
        composite.setLayoutData(data);

     

        //Field label
        Label label = new Label(composite, SWT.NONE);
        label.setText(option.getLabel());
        GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.CENTER;
        gd.grabExcessHorizontalSpace = false;
        gd.grabExcessVerticalSpace = false;
        gd.widthHint =convertWidthInCharsToPixels(label.getText().length() + 4);        
        label.setLayoutData(gd);

        valueField = new Text(composite, SWT.SINGLE | SWT.BORDER);
        gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        gd.widthHint = convertWidthInCharsToPixels(TEXT_FIELD_WIDTH);
        valueField.setLayoutData(gd);
        valueField.addModifyListener(new ModifyListener() {
            String old="";
            public void modifyText(ModifyEvent e) {
                if(old.equals(e.data)) {
                    return;
                }
                firePropertyChange(old,(String) e.data);
                old=""+e.data;
            }
        });
        
        
        button = new Button (composite, SWT.PUSH);
        button.setText("Browse");
         gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.CENTER;
        gd.grabExcessHorizontalSpace = false;
        gd.grabExcessVerticalSpace = false;
        gd.widthHint =convertWidthInCharsToPixels(button.getText().length() + 2);        
        button.setLayoutData(gd);
        button.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {
               FileDialog d = new FileDialog(parent.getShell());
               if(getValue()!=null){
                   d.setFilterPath(getValue());
               }
               d.setText(option.getLabel());
               String s = d.open();
               if(s!=null){
                   setValue(s);
               }
            }

            public void widgetDefaultSelected(SelectionEvent e) {               
                widgetSelected(e);
            }
        });

    }

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#setValue(java.lang.String)
     */
    public void setValue(String value) {
        valueField.setText(value);
    }

    
    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#getValue()
     */
    public String getValue() {      
        return valueField.getText();
    }

}

/*
 */
package org.cs3.jlmp.internal.properties;



import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.widgets.Control;

/**
 */
public interface PropertyEditor {
    public Control getControl();
    public String getValue();
    public String getKey();
    public void setValue(String value);
    public String validate();
    public boolean isEnabled();
    public void setEnabled(boolean enabled);
    public void addPropertyChangeListener(IPropertyChangeListener l);
    public void removePropertyChangeListener(IPropertyChangeListener l);
    public void revertToDefault();
}

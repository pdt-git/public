/* $LICENSE_MSG$(ld) */

/*
 */
package org.cs3.prolog.ui.util;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.prolog.common.Option;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


abstract public class OptionEditor implements PropertyEditor {
    Composite parent;

    Composite control;

    Option option;

    private Vector<IPropertyChangeListener> listeners = new Vector<IPropertyChangeListener>();

    /**
     * @param parent
     * @param option
     */
    public OptionEditor(Composite parent, Option option) {
        super();
        this.parent = parent;
        this.option = option;
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#getControl()
     */
    @Override
	public Control getControl() {
        if (control == null) {
            control = new Composite(parent, SWT.NONE);
            createControls(control);
        }
        return control;
    }

    /**
     * @param control2
     */
    protected abstract void createControls(Composite composite);

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#isEnabled()
     */
    @Override
	public boolean isEnabled() {
        return getControl().isEnabled();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#setEnabled(boolean)
     */
    @Override
	public void setEnabled(boolean enabled) {
        getControl().setEnabled(enabled);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#setPropertyChangeListener(java.beans.PropertyChangeListener)
     */
    @Override
	public void addPropertyChangeListener(IPropertyChangeListener l) {
        synchronized (listeners) {
            if (!listeners.contains(l)) {
                listeners.add(l);
            }
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#removePropertyChangeListener(java.beans.PropertyChangeListener)
     */
    @Override
	public void removePropertyChangeListener(IPropertyChangeListener l) {
        synchronized (listeners) {
            if (listeners.contains(l)) {
                listeners.remove(l);
            }
        }
    }
    
    protected void firePropertyChange(String oldValue, String newValue){
        PropertyChangeEvent e = new PropertyChangeEvent(this,getKey(),oldValue,newValue);
        Vector<IPropertyChangeListener> cloned = getAListenerClone();
        for (Iterator<IPropertyChangeListener> it = cloned.iterator(); it.hasNext();) {
            IPropertyChangeListener l = it.next();
            l.propertyChange(e);
        }
    }
    
	@SuppressWarnings("unchecked")
	private Vector<IPropertyChangeListener> getAListenerClone() {
		Vector<IPropertyChangeListener> cloned=null;
        synchronized(listeners){
            cloned= (Vector<IPropertyChangeListener>) listeners.clone();
        }
		return cloned;
	}
	
    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#getOption()
     */
    @Override
	public String getKey() {
      return option.getId();
    }
    
    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#revertToDefault()
     */
    @Override
	public void revertToDefault() {
      setValue(option.getDefault());
    }

    protected int convertWidthInCharsToPixels(int chars){
        GC gc = new GC(parent);        
        int c=Dialog.convertWidthInCharsToPixels(gc.getFontMetrics(),chars);
        gc.dispose();
        return c;
         
    }
    
     

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.internal.properties.PropertyEditor#validate()
     */
    @Override
	public String validate() {        
        	return option.validate(getValue());        
    }

    public static PropertyEditor create(Composite parent,Option o){
        switch(o.getType()){
        case Option.DIR:
            return new DirectoryEditor(parent,o);        
        case Option.FILE:
            return new FileEditor(parent,o);
//        case Option.FILES:
        case Option.DIRS:
            return new FileListEditorAdapter(parent,o);            
        case Option.FLAG:
            return new FlagEditor(parent,o);
        case Option.ENUM:
            return new EnumEditor(parent,o);            
        default:
            return new StringEditor(parent,o);
        }
                
    }
}


/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pdt.ui.util;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Option;
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
        Vector<IPropertyChangeListener> cloned=null;
        synchronized(listeners){
            cloned= (Vector<IPropertyChangeListener>) listeners.clone();
        }
        for (Iterator<IPropertyChangeListener> it = cloned.iterator(); it.hasNext();) {
            IPropertyChangeListener l = it.next();
            l.propertyChange(e);
        }
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
/*
 * Created on 05.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl;

import org.cs3.pl.extension.IJTransformerObserver;
import org.eclipse.jface.dialogs.MessageDialog;

/**
 * @author windeln
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TestObserver implements IJTransformerObserver {
    
    public TestObserver() {
        System.out.println("testobserver init");
        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.extension.IFactbaseUpdatedObserver#update()
     */
    public void update(int kind, Object[] info) {
        System.out.println("UPDATE");

    }

}

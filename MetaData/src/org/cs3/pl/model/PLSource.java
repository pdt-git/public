/*
 */
package org.cs3.pl.model;

import java.util.List;

/**
 */
public class PLSource implements IPrologElement {

    /**
     * 
     */
    public PLSource() {
        super();
        // TODO Auto-generated constructor stub
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor, java.util.List, java.lang.Object)
     */
    public void accept(IPrologElementVisitor visitor, List path, Object role) {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor)
     */
    public void accept(IPrologElementVisitor visitor) {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#isSynthetic()
     */
    public boolean isSynthetic() {
        // TODO Auto-generated method stub
        return false;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#getLabel()
     */
    public String getLabel() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#getSource()
     */
    public ISource getSource() {
        // TODO Auto-generated method stub
        return null;
    }

}

/*
 */
package org.cs3.pl.model.internal.runtime;

import java.io.InputStream;
import java.util.List;

import org.cs3.pl.model.IClause;
import org.cs3.pl.model.IModule;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ISource;
import org.cs3.pl.model.ISourceFile;

/**
 */
public class SourceFile implements ISourceFile{

    
    /**
     * @param module
     * @param filename
     */
    public SourceFile(Module module, String filename) {
        
        // TODO Auto-generated constructor stub
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISourceFile#getContents()
     */
    public InputStream getContents() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISourceFile#getContents(int, int)
     */
    public String getContents(int offset, int length) {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISourceFile#getModule()
     */
    public IModule getModule() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISourceFile#getClauses()
     */
    public IClause[] getClauses() {
        // TODO Auto-generated method stub
        return null;
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
     * @see org.cs3.pl.model.IPrologElement#getLable()
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

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor, java.util.List, java.lang.Object)
     */
    public void accept(IPrologElementVisitor visitor, List path, Object role) {
        // TODO Auto-generated method stub
        
    }

}

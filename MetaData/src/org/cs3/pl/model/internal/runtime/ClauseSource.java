/*
 */
package org.cs3.pl.model.internal.runtime;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;

import org.cs3.pl.model.IClause;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ISource;
import org.cs3.pl.model.ISourceFile;
import org.cs3.pl.model.PLRuntime;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class ClauseSource implements ISource{

    
    
    private IClause clause;
    private ISourceFile sourceFile;
    private PLRuntime runtime;
    private Integer row;

    public ClauseSource(PLRuntime runtime, IClause clause) {
        this.clause=clause;
        this.runtime=runtime;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISource#getSourceFile()
     */
    public ISourceFile getSourceFile() {
        if(sourceFile==null){
            sourceFile=querySourceFile();
        }
        return sourceFile;
    }

    /**
     * @return
     */
    private ISourceFile querySourceFile() {
        String symbol = queryProperty("file");
        return runtime.getSourceFile(symbol);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISource#getOffset()
     */
    public int getOffset() {
      return -1;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISource#getLength()
     */
    public int getLength() {
      return -1;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISource#getRow()
     */
    public int getRow() {
        if(row==null){
            row=queryRow();
        }
        return row==null?-1:row.intValue();
    }

    /**
     * @return
     */
    private Integer queryRow() {
        String r = queryProperty("line_count");
        return Integer.decode(r);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ISource#getColumn()
     */
    public int getColumn() {
       return -1;
    }

  

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor, java.util.List, java.lang.Object)
     */
    public void accept(IPrologElementVisitor visitor, List path, Object role) {
        if(path.contains(this)){
            return;
        }
        if(!visitor.visit(this,path,role)){
            return;
        }
        path.add(this);
        getSourceFile().accept(visitor,path,"sourceFile");
        clause.accept( visitor,path,"clause");        
        path.remove(path.size()-1);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor)
     */
    public void accept(IPrologElementVisitor visitor) {        
        accept(visitor,new LinkedList(),null);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#isSynthetic()
     */
    public boolean isSynthetic() {
        return false;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#getLable()
     */
    public String getLabel() {
        ISourceFile sourceFile = getSourceFile();
        return sourceFile==null ? null : sourceFile.getLabel()+":"+getRow();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#getSource()
     */
    public ISource getSource() {
        return this;
    }
    protected String queryProperty(String propertyName) {
        IPrologInterface pif = runtime.getPrologInterface();
        PrologSession s = pif.getSession();
        try {
            Hashtable r = s.query("clause_properties(" + this.toString() + ","
                    + propertyName + "(A))");
            return (String) r.get("A");
        } finally {
            s.dispose();
        }
    }
    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if(!(obj instanceof ClauseSource)){
            return false;
        }
        ClauseSource m = (ClauseSource) obj;
        if(m==null){
            return false;
        }
        return this.clause.equals(m.clause);
    }
    
    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return clause.hashCode();
    }
}

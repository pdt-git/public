/*
 */
package org.cs3.pl.model.internal.runtime;

import java.util.LinkedList;

import org.cs3.pl.model.IPrologElement;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ISource;
import org.cs3.pl.model.PLRuntime;

/**
 */
public abstract class AbstractTerm implements IPrologElement{

    /**
     * @param runtime2
     * @param parent2
     * @param string
     */
    public AbstractTerm(PLRuntime runtime, IPrologElement parent, String string) {
        this.runtime=runtime;
        this.parent=parent;
        this.term=string;
    }

    public String getLabel() {
       return getTermString();
    }

    public void accept(IPrologElementVisitor visitor, java.util.List path, Object role) {
        if(path.contains(this)){
            return;
        }
        if(!visitor.visit(this,path,role)){
            return;
        }
        path.add(this);
        if(parent!=null){
            parent.accept(visitor,path,"parent");    
        }
    
        path.remove(path.size()-1);
    
    }

    public String getTermString() {
       return term;
    }

    public int hashCode() {
        return term.hashCode();
    }

    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof List)) {
            return false;
        }
        List m = (List) obj;
        
        return this.runtime == m.runtime
        && parent.equals(m.parent)
        && term.equals(m.term);
        		
    }

    public ISource getSource() {      
        return null;
    }

    public boolean isSynthetic() {    
        return false;
    }

    public void accept(IPrologElementVisitor visitor) {
      accept(visitor,new LinkedList(),null);
    }

    protected PLRuntime runtime;
    protected IPrologElement parent;
    protected String term;

   

}

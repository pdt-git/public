/*
 */
package org.cs3.pl.model.internal.runtime;

import java.util.Hashtable;

import org.cs3.pl.model.IList;
import org.cs3.pl.model.IPrologElement;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ITerm;
import org.cs3.pl.model.PLRuntime;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class List extends AbstractTerm implements IList {

    private ITerm[] head;

    /**
     * 
     */
    public List(PLRuntime runtime, IPrologElement parent, String term) {
       super(runtime,parent,term);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IList#getHead()
     */
    public ITerm[] getHead() {
        if(head==null){
            head=queryHead();
        }
        return head;
    }

    /**
     * @return
     */
    private ITerm[] queryHead() {
        PrologSession s =runtime.getPrologInterface().getSession();
        try{
            Hashtable[] r = s.queryAll("member(A,"+term+")");
            ITerm[] result = new ITerm[r.length];
            for (int i = 0; i < result.length; i++) {
                result[i]=runtime.parseTerm(this,(String) r[i].get("A"));
            }
            return result;
        }
        finally{
            s.dispose();
        }        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IList#getTail()
     */
    public IList getTail() {    
        return null;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor, java.util.List, java.lang.Object)
     */
    public void accept(IPrologElementVisitor visitor, java.util.List path,
            Object role) {
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
                
        ITerm[] elms= getHead();
        for (int i = 0; i < elms.length; i++) {
            elms[i].accept(visitor,path,"element");
        }
        path.remove(path.size()-1);

    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IPrologElement#getLable()
     */
    public String getLabel() {
        return term;
    }

}

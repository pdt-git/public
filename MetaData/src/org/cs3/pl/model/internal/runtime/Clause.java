/*
 */
package org.cs3.pl.model.internal.runtime;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;

import org.cs3.pl.model.IClause;
import org.cs3.pl.model.IPredicate;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ISource;
import org.cs3.pl.model.ITerm;
import org.cs3.pl.model.PLRuntime;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class Clause implements IClause {

    private ITerm body;

    private ITerm head;

    private Integer index;

    private IPredicate predicate;

    private String ref;

    private PLRuntime runtime;

    private Integer type;

    private ISource source;

    /**
     * @param runtime
     * @param predicate
     * @param index
     * @param ref
     */
    public Clause(PLRuntime runtime, IPredicate predicate, int index, String ref) {

        this.runtime = runtime;
        this.predicate = predicate;
        this.index = new Integer(index);
        this.ref = ref;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor)
     */
    public void accept(IPrologElementVisitor visitor) {
        accept(visitor,new LinkedList(),null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor,
     *           java.util.List)
     */
    public void accept(IPrologElementVisitor visitor, List path, Object role) {
        if(path.contains(this)){
            return;
        }
        if(!visitor.visit(this,path,role)){
            return;
        }
        path.add(this);
        if(getPredicate()!=null){
            getPredicate().accept(visitor,path,"predicate");
        }
        if(getHead()!=null){
            getHead().accept(visitor,path,"head");
        }
        if(getBody()!=null){
            getBody().accept(visitor,path,"body");
        }
        if(getSource()!=null){
            getSource().accept(visitor,path,"source");
        }
        path.remove(path.size()-1);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IClause#getBody()
     */
    public ITerm getBody() {
        if (body == null) {
          queryHeadAndBody();
        }
        return body;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IClause#getClauseType()
     */
    public int getClauseType() {
        if (type == null) {
            type = new Integer(queryBooleanProperty("fact") ? FACT : RULE);
        }
        return type.intValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IClause#getHead()
     */
    public ITerm getHead() {
        if (head == null) {
          queryHeadAndBody();
        }
        return head;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#getLable()
     */
    public String getLabel() {
        return getHead().getTermString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IClause#getPredicate()
     */
    public IPredicate getPredicate() {
        if (predicate == null) {
            predicate = queryPredicate();
        }
        return predicate;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#getSource()
     */
    public ISource getSource() {
        if (source == null) {
            source = querySource();
        }
        return source;
    }

  

    /**
     * @return
     */
    private ISource querySource() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#isSynthetic()
     */
    public boolean isSynthetic() {      
        return false;
    }

  
    protected boolean queryBooleanProperty(String propertyName) {
        IPrologInterface pif = runtime.getPrologInterface();
        PrologSession s = pif.getSession();
        try {
            Hashtable r = s.query("clause_properties(" + this.toString() + ","
                    + propertyName + ")");
            return r != null;
        } finally {
            s.dispose();
        }

    }

    /**
     * @return
     */
    private void queryHeadAndBody() {
        IPrologInterface pif = runtime.getPrologInterface();
        PrologSession s = pif.getSession();
        try {
            Hashtable r = s.query("clause(LongHead,Body,"+getClauseReference()+"),strip_module(LongHead,_,Head)");
            head= runtime.parseTerm(this,(String) r.get("Head"));
            body= runtime.parseTerm(this,(String) r.get("Body"));
        } finally {
            s.dispose();
        }


    }

    /**
     * @return
     */
    private IPredicate queryPredicate() {
        // TODO Auto-generated method stub
        return null;
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
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof Clause)) {
            return false;
        }
        Clause m = (Clause) obj;
        return this.runtime == m.runtime
        			&&this.ref==m.ref;

    }

    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return ref.hashCode();
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.model.IClause#getClauseReference()
     */
    public String getClauseReference() {
        return ref;
    }
}
/*
 */
package org.cs3.pl.model.internal.runtime;

import java.util.Hashtable;
import java.util.List;

import org.cs3.pl.model.ICompound;
import org.cs3.pl.model.IOperator;
import org.cs3.pl.model.IPrologElement;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ITerm;
import org.cs3.pl.model.PLRuntime;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class Compound extends AbstractTerm implements ICompound {

    private String opName;

    private Integer arity;

    private ITerm[] operands;

    /**
     * @param string
     * @param parent
     * @param runtime
     *  
     */
    public Compound(PLRuntime runtime, IPrologElement parent, String term) {
       super(runtime,parent,term);
    }

  

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.ICompound#getFunctor()
     */
    public String getFunctor() {
        return getOperatorName()+"/"+getArity();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.ICompound#getOperator()
     */
    public String getOperatorName() {
        if(opName==null){
            queryNameAndArity();
        }
        return opName;
    }

    /**
     * 
     */
    private void queryNameAndArity() {
        PrologSession s = runtime.getPrologInterface().getSession();
        try{
            Hashtable r = s.query("functor("+term+",Name,Arity)");
            opName=(String) r.get("Name");
            arity=Integer.decode((String) r.get("Arity"));
        }
        finally{
            s.dispose();
        }
    }



    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.ICompound#getOperants()
     */
    public ITerm[] getOperands() {
       if(operands==null){
           operands=queryOperands();
       }
        return operands;
    }

    /**
     * @return
     */
    private ITerm[] queryOperands() {
        PrologSession s = runtime.getPrologInterface().getSession();
        try{
            Hashtable[] r = s.queryAll("arg(I,"+term+",A)");
            ITerm[] result = new ITerm[r.length];
            for (int i = 0; i < result.length; i++) {
                int index = Integer.parseInt((String) r[i].get("I"))-1;
                result[index]=runtime.parseTerm(this,(String) r[i].get("A"));
            }
            return result;
        }
        finally{
            s.dispose();
        }
    }



    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.ICompound#getArity()
     */
    public int getArity() {
        if(arity==null){
            queryNameAndArity();
        }
         return arity.intValue();
     }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor,
     *           java.util.List, java.lang.Object)
     */
    public void accept(IPrologElementVisitor visitor, List path, Object role) {
        if (path.contains(this)) {
            return;
        }
        if(!visitor.visit(this,path,role)){
            return;
        }
        path.add(this);
        if(getOperator()!=null){
            getOperator().accept(visitor,path,"operator");
        }
        ITerm[] post = getOperands();
        for (int i = 0; i < post.length; i++) {
            post[i].accept(visitor, path,"operand");
        }
        path.remove(path.size() - 1);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#getLable()
     */
    public String getLabel() {        
        return getFunctor();
    }
    
    

    /* (non-Javadoc)
     * @see org.cs3.pl.model.ICompound#getOperator()
     */
    public IOperator getOperator() {
        // TODO implement it.
        return null;
    }
}
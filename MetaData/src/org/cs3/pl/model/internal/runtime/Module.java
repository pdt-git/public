/*
 */
package org.cs3.pl.model.internal.runtime;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;

import org.cs3.pl.model.IModule;
import org.cs3.pl.model.IPredicate;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ISource;
import org.cs3.pl.model.ISourceFile;
import org.cs3.pl.model.PLRuntime;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class Module implements IModule {

    String moduleName;

    ISourceFile sourceFile;

    private PLRuntime runtime;

    private PrologSession s;

    private IPredicate[] predicates;

    /**
     * @param pif
     * @param moduleName
     */
    public Module(PLRuntime runtime, String moduleName) {
        super();
        this.moduleName = moduleName;
        this.runtime = runtime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IModule#getPredicates()
     */
    public IPredicate[] getPredicates() {
        if (predicates == null) {
            IPrologInterface pif = runtime.getPrologInterface();
            s = pif.getSession();
            try {
                s.query("style_check(+dollar)");
                Hashtable[] r = s.queryAll("predicates_defined_in("
                        + moduleName + ",_,Name,Arity,Exported)");
                predicates = new IPredicate[r.length];
                for (int i = 0; i < r.length; i++) {
                    int arity = Integer.parseInt((String) r[i].get("Arity"));
                    String name = (String) r[i].get("Name");
                    Boolean exported = Boolean.valueOf((String) r[i].get("Exported"));
                    predicates[i] = new Predicate(runtime, this, name, arity,exported);
                }
            } finally {
                s.dispose();
            }
        }
        return predicates;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor,
     *           java.util.List)
     */
    public void accept(IPrologElementVisitor visitor, List path,Object role) {
        if (path.contains(this)) {
            return;
        }
        if(!visitor.visit(this,path,role)){
            return;
        }
        path.add(this);
        if(getSourceFile()!=null){
            getSourceFile().accept(visitor,path,"sourceFile");
        }
        IPredicate[] post = getPredicates();
        for (int i = 0; i < post.length; i++) {
            post[i].accept(visitor, path,"predicate");
        }
        path.remove(path.size() - 1);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor)
     */
    public void accept(IPrologElementVisitor visitor) {
        accept(visitor, new LinkedList(),null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#isSynthetic()
     */
    public boolean isSynthetic() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#getLable()
     */
    public String getLabel() {
        return moduleName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#getSource()
     */
    public ISource getSource() {
        return null;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return moduleName.hashCode();
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof Module)) {
            return false;
        }
        Module m = (Module) obj;
        return this.runtime == m.runtime
                && this.moduleName.equals(m.moduleName);

    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model.IModule#getSourceFile()
     */
    public ISourceFile getSourceFile() {
        if(sourceFile==null){
            IPrologInterface pif = runtime.getPrologInterface();
            s = pif.getSession();
            try {
               
                Hashtable r = s.query("current_module("+moduleName+", A)");
                if(r!=null){
                    String filename = (String) r.get("A");
                    sourceFile=runtime.getSourceFile(filename);
                }
            } finally {
                s.dispose();
            }
        }
        return sourceFile;
    }
}

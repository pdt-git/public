/*
 */
package org.cs3.pl.model.internal.runtime;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;

import org.cs3.pl.model.IClause;
import org.cs3.pl.model.IModule;
import org.cs3.pl.model.IPredicate;
import org.cs3.pl.model.IPrologElementVisitor;
import org.cs3.pl.model.ISource;
import org.cs3.pl.model.PLRuntime;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class Predicate implements IPredicate {

    private IModule module;

    private PLRuntime runtime;

    private Boolean multifile;

    private Boolean dynamic;

    private Boolean exported;

    private IClause[] clauses;

    private String name;

    private int arity;

    private String functor;

    /**
     * @param runtime
     * @param module
     * @param hashtable
     */
    public Predicate(PLRuntime runtime, IModule module, String name, int arity) {
        this.runtime = runtime;
        this.module = module;
        this.name = name;
        this.arity = arity;
    }
    
    /*
     * ld: this is for performance reason:
     * In a typical cenario (expanding child nodes in a tree view),
     * the exported property will be queried for each child predicate of a parent module
     * since it determines the icon for the child predicate. 
     * Right now this is rather costy. otoh, if we can queryall the exported 
     * solutions when creating the child nodes, we only need one query all and pass it 
     * with the constructor.
     * Note that this is an ad-hoc solution that should be replaced as soon as  possible.
     */
    public Predicate(PLRuntime runtime, IModule module, String name, int arity,Boolean exported) {
        this.runtime = runtime;
        this.module = module;
        this.name = name;
        this.arity = arity;
        this.exported=exported;

    }
    public String getHeadString() {
        StringBuffer b = new StringBuffer();
        b.append(this.getName());
        if (getArity() > 0) {
            b.append("(");
            for (int i = 0; i < getArity(); i++) {
                if (i > 0) {
                    b.append(", ");
                }
                b.append("_");
            }
            b.append(")");
        }
        return b.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPredicate#getClauses()
     */
    public IClause[] getClauses() {
        IPrologInterface pif = runtime.getPrologInterface();
        PrologSession s = pif.getSession();
        if (clauses == null) {
            try {
                Hashtable[] r = s.queryAll(getModule().getLabel()+":nth_clause(" + this.getHeadString()
                        + ",Index,Reference)");
                clauses = new IClause[r.length];
                for (int i = 0; i < r.length; i++) {
                    int index = Integer.parseInt((String) r[i].get("Index")) - 1;
                    String ref = (String) r[i].get("Reference");
                    clauses[index] = new Clause(runtime, this, index+1, ref);
                }
            } finally {
                s.dispose();
            }
        }
        return clauses;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPredicate#getModule()
     */
    public IModule getModule() {
        return module;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPredicate#isMultifile()
     */
    public boolean isMultifile() {
        if (multifile == null) {
            multifile = new Boolean(queryBooleanProperty("multifile"));
        }
        return multifile.booleanValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPredicate#isDynamic()
     */
    public boolean isDynamic() {
        if (dynamic == null) {
            dynamic = new Boolean(queryBooleanProperty("dynamic"));
        }
        return dynamic.booleanValue();
    }

    protected boolean queryBooleanProperty(String propertyName) {
        IPrologInterface pif = runtime.getPrologInterface();
        PrologSession s = pif.getSession();
        try {
            Hashtable r = s.query(getModule().getLabel()+":predicate_property(" 
                    +this.getHeadString()
                    + "," + propertyName + ")");
            return r != null;
        } finally {
            s.dispose();
        }

    }

    protected String queryProperty(String propertyName) {
        IPrologInterface pif = runtime.getPrologInterface();
        PrologSession s = pif.getSession();
        try {
            Hashtable r = s.query(getModule().getLabel()+":predicate_property("
                    +this.getHeadString() + "," + propertyName + "(A))");
            return (String) r.get("A");
        } finally {
            s.dispose();
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPredicate#isPublic()
     */
    public boolean isExported() {
        if (exported == null) {
            exported = new Boolean(queryBooleanProperty("exported"));
        }
        return exported.booleanValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor,
     *           java.util.List)
     */
    public void accept(IPrologElementVisitor visitor, List path, Object role) {
        if (path.contains(this)) {
            return;
        }
        if (!visitor.visit(this, path, role)) {
            return;
        }
        path.add(this);
        getModule().accept(visitor, path, "module");

        IClause[] post = getClauses();
        for (int i = 0; i < post.length; i++) {
            post[i].accept(visitor, path, "clause");
        }
        path.remove(path.size() - 1);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor)
     */
    public void accept(IPrologElementVisitor visitor) {
        accept(visitor, new LinkedList(), null);
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
        return getFunctor();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#getSource()
     */
    public ISource getSource() {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPredicate#getName()
     */
    public String getName() {
        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPredicate#getArity()
     */
    public int getArity() {
        return arity;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPredicate#getFunctor()
     */
    public String getFunctor() {
        if (functor == null) {
            functor = name + "/" + arity;
        }
        return functor;
    }
    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
       return getModule().hashCode()+getFunctor().hashCode();
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
        if (!(obj instanceof Predicate)) {
            return false;
        }
        Predicate m = (Predicate) obj;
        return this.runtime == m.runtime
                && this.getFunctor().equals(m.getFunctor())
                && this.getModule().equals(m.getModule());

    }

}

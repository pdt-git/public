/*
 */
package org.cs3.pl.model;

import java.util.Hashtable;
import java.util.LinkedList;

import org.cs3.pl.model.internal.runtime.Atom;
import org.cs3.pl.model.internal.runtime.Compound;
import org.cs3.pl.model.internal.runtime.List;
import org.cs3.pl.model.internal.runtime.Module;
import org.cs3.pl.model.internal.runtime.PLString;
import org.cs3.pl.model.internal.runtime.Variable;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class PLRuntime implements IPrologElement {

    private ISourceProvider sourceFileFactory;

    private IPrologInterface pif;

    private IPrologElement[] modules;

    /**
     * @param pif
     */
    public PLRuntime(IPrologInterface pif) {
        super();
        this.pif = pif;
    }
    public ISourceFile getSourceFile(String symbolicName) {
        //return sourceFileFactory.createSourceFile(symbolicName,module);
        return null;
    }

    public ITerm parseTerm(IPrologElement parent, String string) {
        PrologSession s = pif.getSession();
        String type=null;
        try {
            Hashtable r = s.query("term_type(" + string + ",A)");
            
            type = (String) r.get("A");
            if ("list".equals(type)) {
                return new List(this, parent, string);
            } else if ("compound".equals(type)) {
                return new Compound(this, parent, string);
            }
            else if ("string".equals(type)) {
                return new PLString(this, parent, string);
            }
            else if ("var".equals(type)) {
                return new Variable(this, parent, string);
            }
            else if ("float".equals(type)
                   ||"integer".equals(type)
                   || "atom".equals(type)) {
                return new Atom(this, parent, string);
            }
        } finally {
            s.dispose();
        }
        throw new RuntimeException("cannot handle term type: "+type);

    }

    public IPrologInterface getPrologInterface() {
        return pif;
    }

    public void setPif(IPrologInterface pif) {
        this.pif = pif;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#accept(org.cs3.pl.model.IPrologElementVisitor,
     *           java.util.List, java.lang.Object)
     */
    public void accept(IPrologElementVisitor visitor, java.util.List path,
            Object role) {
        if (path.contains(this)) {
            return;
        }
        if (!visitor.visit(this, path, role)) {
            return;
        }
        path.add(this);
        IPrologElement[] post = getModules();
        for (int i = 0; i < post.length; i++) {
            post[i].accept(visitor, path, "module");
        }
        path.remove(path.size() - 1);

    }

    /**
     * @return
     */
    public IPrologElement[] getModules() {
        if (modules == null) {
            modules = queryModules();
        }
        return modules;
    }

    /**
     * @return
     */
    private IPrologElement[] queryModules() {
       PrologSession s = pif.getSession();
       try{
           Hashtable[] r = s.queryAll("style_check(+dollar),current_module(A)");
           IPrologElement[] p = new IPrologElement[r.length];
           for (int i = 0; i < r.length; i++) {
               String name = (String) r[i].get("A");
               p[i]=new Module(this,name);            
           }
           return p;
       }
       finally{
           s.dispose();
       }
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
        return "Prolog Runtime";
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.model.IPrologElement#getSource()
     */
    public ISource getSource() {
        return null;
    }
}

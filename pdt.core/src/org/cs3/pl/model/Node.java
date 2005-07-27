/*
 */
package org.cs3.pl.model;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class Node implements INode{

    protected PrologInterface pif;
    protected String id;
    protected Map properties;
    protected List children;

    /**
     * 
     */
    public Node(PrologInterface pif, String Id, Map properties) {
        this.pif=pif;
        this.id=Id;
        this.properties=properties;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getId()
     */
    public String getId() {
        return id;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getParent()
     */
    public String getParent() {
        return (String) properties.get("parent");
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getChildren()
     */
    public List getChildren() {
       if(children==null){
           children=generateChildren();
       }
        return children;
    }

    public static List find(PrologInterface pif, String properties){
        Vector result = new Vector();
        PrologSession s = pif.getSession();
        if(!properties.trim().startsWith("[")){
            properties="["+properties+"]";
        }
        try{
            List  r = s.queryAll("node(Id,"+properties+"),node(Id,List)");
            for (Iterator it = r.iterator(); it.hasNext();) {
                Map ri = (Map) it.next();
                String childId=(String) ri.get("Id");
                String childPropList = (String) ri.get("List");
                List rr = s.queryAll("member(_M,"+childPropList+"),parse_property(_M,Key->Value)");
                HashMap childProperties = new HashMap();
                for (Iterator itt = rr.iterator(); it.hasNext();) {
                    Map rri = (Map) itt.next();
                    String key=(String) rri.get("Key");
                    String value=(String) rri.get("Value");
                    childProperties.put(key,value);
                }
                result.add(new Node(pif,childId,childProperties));
            }            
        }
        finally{
            s.dispose();
        }
        return result;
    }
    /**
     * @return
     */
    protected List generateChildren() {        
        return find(pif,"parent("+id+")");
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#getProperties()
     */
    public Map getProperties() {
        return properties;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if(!(obj instanceof Node)){
            return false;
        }
        return ((Node)obj).getId().equals(id);
    }
    
    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return id.hashCode();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.model2.INode#hasChildren()
     */
    public boolean hasChildren() {
        Boolean leaf = Boolean.valueOf((String) getProperties().get("leaf"));
        return !leaf.booleanValue();
    }
}

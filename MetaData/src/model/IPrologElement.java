/*
 */
package model;

import java.util.List;

/**
 */
public interface IPrologElement {    
  
    
    /**
     * accept a listener coming along a given path.
     * @param visitor the visitor
     * @param path the nodes already visited by the visitor.
     */
    public void accept(IPrologElementVisitor visitor, List path);
    
    /**
     * same as <code>accept(visitor, emptyList)</code>.
     * @param visitor
     */
    public void accept(IPrologElementVisitor visitor);
    
    /**
     * Detect wether this element is synthetic.
     * Synthetic elements fill conceptual gaps in the model.
     * An element is considered synthetic, if it does not by itself correspond
     * to any data, consulted or unconsulted.
     * <p> e.g. the <code> Runtime </code> Project does not
     * exist as a real project but simply contains predicates that are not 
     * defined in any of the projects known to the model. Still these Predicates
     * exist in the active prolog runtime, so they are said to belong to the
     * "Runtime" project.
     * <p>Similary, there may be files (or other units of data) that are known to exist
     * (since they have been consulted) but that do not correspond to any 
     * of the resources known to the model. Such files are concidered synthetic.
     * @return true if this element is synthetic.
     */
    public boolean isSynthetic();
    
    public String getLable();
    
    /**
     * The element's source.
     * If source code is assoziated with this node, it will be returned.
     * If the source code associated with this node is scattered across a single
     * or several source files, this call should return null. Instead, client code
     * should traverse the children (e.g. clauses) to access the source code.
     * @return
     */
    public ISource getSource();
}

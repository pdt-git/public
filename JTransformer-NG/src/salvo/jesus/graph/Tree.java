package salvo.jesus.graph;

import salvo.jesus.graph.*;
import java.util.*;

/**
 * The superinterface of all <tt>Tree</tt>s. This interface
 * abstracts a free tree. A free tree is a tree
 * whereby any of the nodes can be a root node.
 *
 * @author  Jesus M. Salvo Jr.
 */

public interface Tree extends Graph {

    /**
     * Sets the root of the <tt>Tree</tt>. The
     * <tt>Vertex</tt> specified must already be in the <tt>Tree</tt>.
     * Otherwise, a <tt>NoSuchVertexException</tt> is returned.
     */
    public void setRoot( Vertex rootVertex ) throws GraphException;

    /**
     * Returns the current root of the <tt>Tree</tt>. It is
     * possible that this method will return null if the <tt>Tree</tt>
     * is empty.
     */
    public Vertex getRoot();

    /**
     * Returns the parent node of the node specified by the argument.
     * @throws  EmptyTreeException is the <tt>Tree</tt> is empty.
     */
    public Vertex getParent( Vertex vertex ) throws GraphException;

    /**
     * Returns the child nodes of the node specified by the argument.
     * @throws  EmptyTreeException is the <tt>Tree</tt> is empty.
     */
    public List getChildren( Vertex vertex ) throws GraphException;

    /**
     * Returns a new instance of a <tt>Tree</tt> that is rooted
     * from the specified node. This method does not alter the <tt>Tree</tt>
     * itself.
     * <p>
     * Although a new <tt>Tree</tt> is returned,
     * the nodes and edges within the new <tt>Tree</tt> are the same
     * instances as those of the <tt>Tree</tt> from where it was taken from.
     *
     * @param   subTreeRootVertex   The root of the subtree that we want to return
     * @throws  EmptyTreeException is the <tt>Tree</tt> is empty.
     */
    public Tree getSubTree( Vertex subTreeRootVertex ) throws Exception;

    /**
     * Returns the depth of the node in the <tt>Tree</tt>.
     */
    public int getDepth( Vertex node ) throws GraphException;

    /**
     * Returns a <tt>List</tt> of the leaves of ths <tt>Tree</tt>.
     */
    public List getLeaves();

    /**
     * Returns the height of the <tt>Tree</tt>.
     */
    public int  getHeight();

    /**
     * Convenience method to adding nodes in a <tt>Tree</tt>.
     * <p>
     * Adds a node to the <tt>Tree</tt>. The parent node
     * must already be existing in the <tt>Tree</tt> before
     * the child node can be added. To add the root node,
     * specify null as the parent.
     * <p>
     * Note that the parent and child relativity is dependent
     * for whatever is the current root node of the <tt>Tree</tt>.
     * Hence, if childNode later on becomes root via setRoot(), then the
     * parent actually becomes the child of childNode.
     *
     * @returns     The <tt>Edge</tt> created as a result of
     * adding the child to its parent. This may be null if
     * the parent is null.
     * @throws      GraphException if there is already a root or the child node
     * is already existing.
     * @throws      NoSuchVertexException if the parent is not null and is not existing
     */
    public Edge addNode( Vertex parent, Vertex childNode ) throws Exception;

    /**
     * Returns true if the specified node is a leaf.
     * @throws  EmptyTreeException is the <tt>Tree</tt> is empty.
     */
    public boolean isLeaf( Vertex vertex ) throws GraphException;

    /**
     * Factory method that returns a new instance of <tt>TreeImpl</tt>.
     */
    public Tree createTree();


}
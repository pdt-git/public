package salvo.jesus.graph;

import java.util.*;
import salvo.jesus.graph.algorithm.*;
import salvo.jesus.graph.listener.*;

/**
 * An implementation of a <tt>Tree</tt>. This implementation
 * is a free tree and allows zero, one or more child nodes for each node.
 * A free tree is a tree whereby any of the nodes can be a root node.
 * By defauly, the first node added when the tree is empty is the root node.
 * <p>
 * Directions of <tt>Edge</tt>s in tree are generally ignored.
 * Directionality is basically implied when specifying the root
 * of a <tt>Tree</tt>, where such operation requires it.
 * <p>
 * A <tt>Tree</tt> does not allow cycle paths. So any attempts to
 * create a cycle will thrown a <tt>CycleException</tt>.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class TreeImpl extends GraphImpl implements Tree {
    private TreeListener listener;

    /**
     * Creates an instance of a <tt>TreeImpl</tt>
     */
    public TreeImpl() {
        super();
        listener = new TreeListener(this);
    }

    /**
     * @see Tree#setRoot
     */
    public void setRoot( Vertex rootVertex ) throws GraphException {
        listener.setRoot(rootVertex);
    }

    /**
     * @see Tree#getRoot
     */
    public Vertex getRoot() {
        return listener.getRoot();
    }

    /**
     * @see Tree#isLeaf
     */
    public boolean isLeaf( Vertex vertex ) throws GraphException
    {
        return listener.isLeaf(vertex);
    }

    /**
     * @see Tree#getParent
     */
    public Vertex getParent( Vertex vertex ) throws GraphException
    {
        return listener.getParent(vertex);
    }

    /**
     * @see Tree#getChildren
     */
    public List getChildren( Vertex vertex ) throws GraphException
    {
        return listener.getChildren(vertex);
    }

    /**
     * @see Tree#getSubTree
     */
    public Tree getSubTree( Vertex subTreeRootVertex ) throws Exception
    {
        return listener.getSubTree(subTreeRootVertex);
    }

    /**
     * @see Tree#getDepth
     */
    public int getDepth( Vertex node ) throws GraphException
    {
        return listener.getDepth(node);
    }

    /**
     * @see Tree#getLeaves
     */
    public List getLeaves() {
        return listener.getLeaves();
    }

    /**
     * @see Tree#getHeight
     */
    public int getHeight()
    {
        return listener.getHeight();
    }

    // REVIEW jvs 8-Mar-2002 -- Seems like the GraphFactory
    // pattern should be extended to cover this case.
    /**
     * Factory method that returns a new instance of <tt>TreeImpl</tt>.
     */
    public Tree createTree() {
        return new TreeImpl();
    }

    /**
     * @see Tree#addNode
     */
    public Edge addNode( Vertex parent, Vertex childNode ) throws Exception
    {
        if (parent == null) {
            if( this.getRoot() != null )
              throw new IllegalTreeException( "Cannot use null as parent node because root of tree has been set." );
            add(childNode);
            return null;
        } else {
            return addEdge(parent,childNode);
        }
    }
}

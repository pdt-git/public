package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;
import salvo.jesus.graph.algorithm.*;
import java.util.*;

/**
 * TreeListener imposes a tree structure on a Graph.  It can be
 * used as a delegate by any class which wants to implement the
 * Tree interface.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public class TreeListener extends NullGraphListener
{
    // TODO jvs 11-Mar-2002 -- write a custom Tree traversal
    // TODO jvs 11-Mar-2002 -- keep track of the parent for each
    // vertex relative to the current root as an optimization

    /**
     * The graph on which we are listening.
     */
    private Tree m_tree;


    /**
     * Reference to the root of the <tt>Tree</tt>.
     */
    private Vertex m_rootVertex;

    /**
     * Creates a new TreeListener for the given Tree.
     *
     * @param tree the graph to which this listener is to be attached;
     * this constructor will automatically register the listener
     * to receive all events
     *
     */
    public TreeListener(Tree tree)
    {
        m_tree = tree;
        m_tree.addListener(this);
    }

    public void beforeVertexAdded(GraphAddVertexEvent event)
        throws Exception
    {
        // when a vertex is added, either it must be the first vertex,
        // or else it must be added together with an edge
        if (m_tree.getVerticesCount() == 0) {
            // empty tree
            return;
        }
        if (event.getEdge() == null) {
            throw new IllegalTreeException(
                "Isolated vertex cannot be added to existing tree");
        }
        // beforeEdgeAdded will take care of the rest of the checking
    }

    public void afterVertexAdded(GraphAddVertexEvent event)
    {
        // by default, first vertex added becomes root
        if (m_rootVertex == null) {
            m_rootVertex = event.getVertex();
        }
    }

    public void beforeVertexRemoved(GraphRemoveVertexEvent event)
      throws Exception
    {
      // Check if removing the vertex will result in a forest.
      if( !this.m_tree.isLeaf( event.getVertex() ) ) {
        throw new IllegalTreeException();
      }
    }

    public void afterVertexRemoved(GraphRemoveVertexEvent event)
    {
        if (event.getVertex() == m_rootVertex) {
            m_rootVertex = null;
        }
    }

    public void beforeEdgeAdded(GraphAddEdgeEvent event)
        throws Exception
    {
        if (m_tree.getVerticesCount() == 0) {
            // always OK to add first edge
            return;
        }
        Edge edge = event.getEdge();
        if (event.isAddingVertexA() && event.isAddingVertexB()) {
            throw new IllegalTreeException(
                "Isolated edge cannot be added to existing tree");
        }
        if (!event.isAddingVertexA() && !event.isAddingVertexB()) {
            // since both vertices already exist, must be
            // creating a cycle
            throw new CycleException();
        }
    }

    public void beforeEdgeRemoved(GraphRemoveEdgeEvent event)
        throws Exception
    {
        if (event.getVertex() == null) {
            throw new IllegalTreeException(
                "removing Edge would disconnect tree");
        }
        if (m_tree.getDegree(event.getVertex()) > 1) {
            throw new IllegalTreeException(
                "removing Vertex would disconnect tree");
        }
    }

    /**
     * @see Tree#setRoot
     */
    public void setRoot( Vertex rootVertex ) throws GraphException
    {
        if( !m_tree.getVertexSet().contains( rootVertex  )) {
            throw new NoSuchVertexException();
        }
        m_rootVertex = rootVertex;
    }

    /**
     * @see Tree#getRoot
     */
    public Vertex getRoot() {
        return m_rootVertex;
    }

    /**
     * @see Tree#isLeaf
     */
    public boolean isLeaf( Vertex vertex ) throws GraphException {
        if( m_rootVertex == null ) {
            throw new EmptyTreeException();
        }

        // A leaf is a) non-root node that has zero or one incident Edge.
        // or b) a root-node and it has no child nodes.
        if( ( m_tree.getRoot() != vertex && m_tree.getDegree( vertex ) < 2 ) ||
            ( m_tree.getRoot() == vertex && m_tree.getChildren( vertex ).size() == 0 ))
            return true;
        else
            return false;
    }

    /**
     * @see Tree#getParent
     */
    public Vertex getParent( Vertex vertex ) throws GraphException {
        // Check that root has been specified
        if( m_rootVertex == null ) {
            throw new EmptyTreeException();
        }

        List    visited = new ArrayList( 10 );
        GraphTraversal  traversal = new DepthFirstGraphTraversal( m_tree );
        Vertex  parent = null;
        List    adjacentVertices = m_tree.getAdjacentVertices( vertex );

        // Now do a depth-first traversal until we stop at our vertex
        traversal.traverse(
            m_rootVertex, visited, new StopAtVisitor( vertex ));

        // Now starting from the last element in the vertices that were
        // visited, find the first Vertex which is adjacent to the Vertex we
        // are interested in.
        int size = visited.size();
        for( int i = size; i > 0; i-- ) {
            parent = (Vertex) visited.get( i - 1 );
            if( adjacentVertices.contains( parent )) {
                return parent;
            }
        }
        return null;
    }

    /**
     * @see Tree#getChildren
     */
    public List getChildren( Vertex vertex ) throws GraphException {
        // Check that root has been specified
        if( m_rootVertex == null ) {
            throw new EmptyTreeException();
        }

        // The children of a node is all its adjacent vertices
        // minus its parent.
        List children = new ArrayList(m_tree.getAdjacentVertices( vertex ));

        children.remove( getParent( vertex ) );
        return children;
    }

    /**
     * @see Tree#getSubTree
     */
    public Tree getSubTree( Vertex subTreeRootVertex ) throws Exception {
        // Check that root has been specified
        if( m_rootVertex == null ) {
            throw new EmptyTreeException();
        }

        Vertex  parent = getParent( subTreeRootVertex );
        List    visited = new ArrayList( 10 );
        GraphTraversal  traversal = new DepthFirstGraphTraversal( m_tree );

        // Add the parent as visited so that we wont be traversing the parent
        // anymore.  The result of the traversal will be all vertices / nodes
        // that includes the root of the subtree plus all its children
        visited.add( parent );
        traversal.traverse( subTreeRootVertex, visited, new NullVisitor() );
        visited.remove( parent );

        // Find out the edge connecting the subtree's root node and the
        // subtree's root's parent node.
        Edge    edgeToParent = null;
        Edge    currentEdge;
        List    incidentEdges = m_tree.getEdges( subTreeRootVertex );
        for( int i = 0; i < incidentEdges.size(); i++ ) {
            currentEdge = (Edge) incidentEdges.get( i );
            if( currentEdge.getVertexA() == parent
                || currentEdge.getVertexB() == parent )
            {
                edgeToParent = currentEdge;
            }
        }

        // Call factory method to create new tree.
        Tree    subTree = m_tree.createTree();

        // Now add all the edges to the subtree.
        // The nodes of the subtree is basically the result of the traversal's
        // visited List plus all their Edges except for the Edge leading to
        // the parent of the subtree's root node.
        Vertex      currentVertex;
        for( int i = 0; i < visited.size(); i++ ) {
            currentVertex = (Vertex) visited.get( i );

            // Always add the vertex, in case there is only one node in the subtree
            // and therefore no edges.
            subTree.add( currentVertex );

            // Add the Vertex's Edges to the subtree, except for the
            // Edge leading to the parent of the subtree's root node.
            incidentEdges = new ArrayList(m_tree.getEdges( currentVertex ));
            if( currentVertex == subTreeRootVertex && edgeToParent != null ) {
                incidentEdges.remove( edgeToParent );
            }
            for( int j = 0; j < incidentEdges.size(); j++ ) {
                currentEdge = (Edge) incidentEdges.get( j );
                subTree.addEdge( currentEdge );
            }
        }
        return subTree;
    }

    /**
     * @see Tree#getDepth
     */
    public int getDepth( Vertex node ) throws GraphException
    {
        if( !m_tree.getVertexSet().contains( node )) {
            throw new NoSuchVertexException();
        }

        Vertex  parent = getParent( node );
        int     depth = 1;

        while( parent != null ) {
            parent = getParent( parent );
            depth++;
        }

        return depth;
    }

    /**
     * @see Tree#getLeaves
     */
    public List getLeaves() {
        Iterator iterator = m_tree.getVerticesIterator();
        List    leaves = new ArrayList( 10 );
        Vertex  currentVertex;

        while (iterator.hasNext()) {
            currentVertex = (Vertex) iterator.next();
            if( ( m_tree.getRoot() == currentVertex && m_tree.getVerticesCount() == 1 ) ||
                ( m_tree.getRoot() != currentVertex && m_tree.getVerticesCount() > 1 &&
                  m_tree.getDegree( currentVertex ) <= 1 ) ) {
                leaves.add( currentVertex );
            }
        }

        return leaves;
    }

    /**
     * @see Tree#getHeight
     */
    public int getHeight() {
        List    leaves = getLeaves();
        int    size = leaves.size();
        int    maxHeight = 0;
        int    currentVertexHeight = 0;

        for( int i = 0; i < size; i++ ) {
            try {
                currentVertexHeight = getDepth( (Vertex) leaves.get( i ));
            }
            catch( Exception ex ) {
                ex.printStackTrace();
            }
            maxHeight = Math.max( currentVertexHeight, maxHeight );
        }
        leaves = null;

        return maxHeight;
    }
}

// End TreeListener.java

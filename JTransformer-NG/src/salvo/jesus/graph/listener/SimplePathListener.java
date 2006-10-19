package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;
import salvo.jesus.graph.algorithm.*;
import java.util.*;

/**
 * SimplePathListener imposes a simple path structure on a
 * Graph.  It can be used as a delegate by any class which wants to implement
 * the SimplePath interface.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public class SimplePathListener
    extends NullGraphListener
    implements AbstractPathListener
{
    // TODO jvs 11-Mar-2002 -- write a custom SimplePath traversal
    
    /**
     * The graph on which we are listening.
     */
    private SimplePath m_simplePath;

    private Vertex m_first,m_last;
    
    /**
     * Creates a new SimplePathListener for the given SimplePath.
     *
     * @param simplePath the graph to which this listener is to be attached;
     * this constructor will automatically register the listener
     * to receive all events
     * 
     */
    public SimplePathListener(SimplePath simplePath)
    {
        m_simplePath = simplePath;
        if (m_simplePath.getVerticesCount() > 0) {
            m_first = m_simplePath.getFirstVertex();
            m_last = m_simplePath.getLastVertex();
        }
        m_simplePath.addListener(this);
    }

    /**
     * @see Path#getFirstVertex
     */
    public Vertex getFirstVertex() {
        return m_first;
    }

    /**
     * @see Path#getLastVertex
     */
    public Vertex getLastVertex() {
        return m_last;
    }

    protected SimplePath getPath()
    {
        return m_simplePath;
    }
    
    /**
     * @see Path#remove
     */
    public void remove()
        throws Exception
    {
        if (m_last == null) {
            throw new NoSuchElementException();
        }
        m_simplePath.remove(m_last);
    }
    
    public void beforeVertexAdded(GraphAddVertexEvent event)
        throws Exception
    {
        // when adding a vertex, it must either be the first vertex, or else 
        // it must be added together with an edge
        if (m_first == null) {
            return;
        }
        if (event.getEdge() == null) {
            throw new IllegalPathException(
                "adding Vertex would violate path contiguity");
        }
        // beforeEdgeAdded will take care of the rest of the checking
    }
        
    public void afterVertexAdded(GraphAddVertexEvent event)
    {
        if (m_first == null) {
            m_first = m_last = event.getVertex();
        }
        // extension of m_last is always handled by afterEdgeAdded
    }

    // NOTE:  don't need any beforeVertexRemoved() checking, because GraphImpl
    // always removes all edges first, which will catch an illegal vertex
    // removal also
    
    public void afterVertexRemoved(GraphRemoveVertexEvent event)
    {
        if (event.getVertex() == m_first) {
            // the only way this can happen is if the last isolated
            // vertex is being removed
            m_first = null;
            m_last = null;
        }
    }
    
    public void beforeEdgeAdded(GraphAddEdgeEvent event)
        throws Exception
    {
        if (m_simplePath.getVerticesCount() == 0) {
            // always OK to add first edge
            return;
        }
        if (event.isAddingVertexA() && event.isAddingVertexB()) {
            throw new IllegalPathException(
                "Isolated edge cannot be added to path");
        }
        if (!event.isAddingVertexA() && !event.isAddingVertexB()) {
            // since both vertices already exist, must be
            // creating a cycle
            throw new CycleException();
        }
        // the existing vertex must be an endpoint
        int degree;
        if (event.isAddingVertexA()) {
            degree = m_simplePath.getDegree(event.getEdge().getVertexB());
        } else {
            degree = m_simplePath.getDegree(event.getEdge().getVertexA());
        }
        if (degree > 1) {
            throw new IllegalPathException(
                "Edge being added violates path linearity");
        }
    }

    public void afterEdgeAdded(GraphAddEdgeEvent event)
    {
        Edge edge = event.getEdge();

        if (m_first == m_last) {
            // just adding first edge
            m_last = edge.getOppositeVertex(m_first);
        } else {
            // extend either first or last
            Vertex vA = event.getEdge().getVertexA();
            Vertex vB = event.getEdge().getVertexB();
            if (vA == m_first) {
                m_first = vB;
            } else if (vA == m_last) {
                m_last = vB;
            } else if (vB == m_first) {
                m_first = vA;
            } else if (vB == m_last) {
                m_last = vA;
            } else {
                // this should be impossible due to pre-checks in
                // beforeEdgeAdded
                throw new IllegalStateException(
                    "Edge being added violates path linearity");
            }
        }
    }

    protected boolean isEndpoint(Vertex v)
    {
        return (v == m_first) || (v == m_last);
    }
    
    public void beforeEdgeRemoved(GraphRemoveEdgeEvent event)
        throws Exception
    {
        // can never remove an edge without removing an endpoint with it
        if (!isEndpoint(event.getVertex())) {
            throw new IllegalPathException(
                "removing Edge would violate path contiguity");
        }
    }

    public void afterEdgeRemoved(GraphRemoveEdgeEvent event)
    {
        Edge edge = event.getEdge();
        // adjust whichever endpoint is being removed
        if (event.getVertex() == m_first) {
            m_first = edge.getOppositeVertex(m_first);
        } else if (event.getVertex() == m_last) {
            m_last = edge.getOppositeVertex(m_last);
        } else {
            // this should be impossible due to pre-checks in
            // beforeEdgeRemoved
            throw new IllegalStateException(
                "removing Edge would violate path contiguity");
        }
    }
}

// End SimplePathListener.java

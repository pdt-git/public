package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;
import java.util.*;

/**
 * DirectedAcyclicGraphListener enforces the DAG property of a graph.  It can
 * be used as a delegate by any class which wants to implement the
 * DirectedAcyclicGraph interface.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public class DirectedAcyclicGraphListener extends NullGraphListener
{
    /**
     * The graph on which we are listening.
     */
    private DirectedAcyclicGraph m_graph;
    
    /**
     * Creates a new DirectedAcyclicGraphListener for the given graph.
     *
     * @param graph the graph to which this listener is to be attached;
     * this constructor will automatically register the listener
     * to receive all events
     * 
     */
    public DirectedAcyclicGraphListener(DirectedAcyclicGraph graph)
    {
        m_graph = graph;
        m_graph.addListener(this);
    }
    
    public void beforeEdgeAdded(GraphAddEdgeEvent event)
        throws Exception
    {
        // verify that adding this edge will not create a cycle
        DirectedEdge edge = (DirectedEdge) event.getEdge();
        if (event.isAddingVertexA() || event.isAddingVertexB()) {
            // since at least one of the incident vertices is not yet part of
            // the graph, adding the edge can't possibly cause
            // a cycle
            return;
        }
        if (m_graph.isPath(edge.getSink(),edge.getSource())) {
            throw new CycleException();
        }
    }

    /**
     * Implementation for DirectedAcyclicGraph.getRoot().
     */
    public List getRoot( ){
        Iterator iterator;
        List rootVertices;

        rootVertices = new ArrayList();

        iterator = m_graph.getVerticesIterator();
        while( iterator.hasNext() ){
            Vertex v = (Vertex) iterator.next();
            if (m_graph.getIncomingEdges(v).size() == 0) {
                rootVertices.add(v);
            }
        }

        return Collections.unmodifiableList(rootVertices);
    }
    
}

// End DirectedAcyclicGraphListener.java

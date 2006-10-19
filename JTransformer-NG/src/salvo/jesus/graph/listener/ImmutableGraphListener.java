package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;

/**
 * ImmutableGraphListener enforces immutability of a graph by asserting
 * whenever someone tries to change the graph.
 * 
 * @author John V. Sichi
 */
public class ImmutableGraphListener extends NullGraphListener
{
    private Graph m_graph;
    
    /**
     * Creates a new ImmutableGraphListener, making the argument immutable
     * until this Listener is removed
     *
     * @param graph the graph to which this listener is to be attached;
     * this constructor will automatically register the listener
     * to receive all events
     * 
     */
    public ImmutableGraphListener(Graph graph)
    {
        m_graph = graph;
        m_graph.addListener(this);
    }

    public void beforeEdgeAdded(GraphAddEdgeEvent e)
    {
        throw new IllegalStateException("graph is immutable");
    }

    public void beforeVertexAdded(GraphAddVertexEvent e)
    {
        throw new IllegalStateException("graph is immutable");
    }

    public void beforeEdgeRemoved(GraphRemoveEdgeEvent e)
    {
        throw new IllegalStateException("graph is immutable");
    }

    public void beforeVertexRemoved(GraphRemoveVertexEvent e)
    {
        throw new IllegalStateException("graph is immutable");
    }
}

// End ImmutableGraphListener.java

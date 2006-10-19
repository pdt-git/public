package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;

/**
 * NullGraphListener is an implementation of GraphListener which just ignores
 * all events.  Subclasses can override specific events and leave others
 * ignored without having to provide stubs.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public class NullGraphListener implements GraphListener
{
    public void beforeVertexAdded(GraphAddVertexEvent event)
        throws Exception
    {
    }
    
    public void afterVertexAdded(GraphAddVertexEvent event)
    {
    }
    
    public void beforeVertexRemoved(GraphRemoveVertexEvent event)
        throws Exception
    {
    }
    
    public void afterVertexRemoved(GraphRemoveVertexEvent event)
    {
    }
    
    public void beforeEdgeAdded(GraphAddEdgeEvent event)
        throws Exception
    {
    }
    
    public void afterEdgeAdded(GraphAddEdgeEvent event)
    {
    }

    public void beforeEdgeRemoved(GraphRemoveEdgeEvent event)
        throws Exception
    {
    }

    public void afterEdgeRemoved(GraphRemoveEdgeEvent event)
    {
    }
}

// End NullGraphListener.java

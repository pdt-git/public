package salvo.jesus.graph.listener;
import salvo.jesus.graph.*;
import salvo.jesus.graph.algorithm.*;
import java.util.*;

/**
 * CyclePathListener extends SimplePathListener to allow a cycle to be
 * defined.  It can be used as a delegate by any class which wants to implement
 * the CyclePath interface.
 *
 * @author John V. Sichi
 * @version $Id$
 */
public class CyclePathListener extends SimplePathListener
{
    private boolean m_bClosed;
    
    /**
     * Creates a new CyclePathListener for the given graph.
     *
     * @param cyclePath the graph to which this listener is to be attached;
     * this constructor will automatically register the listener
     * to receive all events
     *
     * @param bClosed true if cyclePath is already closed
     */
    public CyclePathListener(CyclePath cyclePath,boolean bClosed)
    {
        super(cyclePath);
        m_bClosed = bClosed;
    }

    public void beforeEdgeAdded(GraphAddEdgeEvent event)
        throws Exception
    {
        // if the edge would complete the cycle, then it is allowed where
        // normally it wouldn't be
        Vertex vA = event.getEdge().getVertexA();
        Vertex vB = event.getEdge().getVertexB();
        if ((vA == getFirstVertex() && vB == getLastVertex())
            || (vB == getFirstVertex() && vA == getLastVertex()))
        {
            // but make sure we're not adding an extra cycle
            if (getPath().getEdgesCount() == 0) {
                // first edge is always OK
                return;
            }
            if (getFirstVertex() != getLastVertex()) {
                // no existing cycle
                return;
            }
        }
        
        // otherwise, let SimplePathListener validate
        super.beforeEdgeAdded(event);
    }
}

// End CyclePathListener.java

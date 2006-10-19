package salvo.jesus.graph;

import java.util.*;

/**
 * This event is used to notify interested parties that an Edge object
 * has been added to a Graph object.
 *
 * @author		Jesus M. Salvo Jr.
 */
public class GraphAddEdgeEvent extends EventObject {
    /**
     * The Edge object that was added to a Graph object
     */
    private Edge edge;

    private boolean addingVertexA;
    private boolean addingVertexB;
    
    /**
     * Creates a GraphAddEdgeEvent object indicating the source of the event
     * and the Edge that was added to a Graph object
     *
     * @param source source of the event. This is usually the Graph object *
     * where the Edge object was added.
     * @param	newedge		Edge object that was added to a Graph object
     * @param addingVertexA see isAddingVertexA()
     * @param addingVertexB see isAddingVertexB()
     */
    public GraphAddEdgeEvent(
        Object source,
        Edge newedge,
        boolean addingVertexA,
        boolean addingVertexB)
    {
        super( source );
        this.edge = newedge;
        this.addingVertexA = addingVertexA;
        this.addingVertexB = addingVertexB;
    }

    /**
     * Returns the Edge object that was added to a Graph object
     *
     * @return		The Edge object added
     */
    public Edge getEdge( ) {
        return this.edge;
    }

    /**
     * @return true if adding this edge is implicitly adding vertex A
     * as well
     */
    public boolean isAddingVertexA()
    {
        return addingVertexA;
    }

    /**
     * @return true if adding this edge is implicitly adding vertex B
     * as well
     */
    public boolean isAddingVertexB()
    {
        return addingVertexB;
    }
}

package salvo.jesus.graph.algorithm;

import salvo.jesus.graph.*;
import java.util.*;

/**
 * A concrete subclass of GraphTraversal that uses depth-first search in
 * traversing a directed graph. Note that the traverse() method will only
 * traverse the connected set to which the Vertex the traversal will start at
 * belongs.
 *
 * Further note that due to the directions of edges, not all vertices may
 * actually be visited by the traversal.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class DepthFirstDirectedGraphTraversal
    extends DepthFirstGraphTraversal
{
    private DirectedGraph dgraph;
    
    /**
     * Creates a DepthFirstDirectedGraphTraversal object that will perform
     * a depth first traversal on the specified DirectedGraph
     *
     * @param   graph   DirectedGraph on which the traversal will be performed.
     */
    public DepthFirstDirectedGraphTraversal( DirectedGraph graph ) {
        super( graph );
        dgraph = graph;
    }
    
    /**
     * Override super to only get the outgoing adjacent vertices.
     */
    protected List getAdjacentVertices(Vertex v)
    {
        return dgraph.getOutgoingAdjacentVertices(v);
    }
}

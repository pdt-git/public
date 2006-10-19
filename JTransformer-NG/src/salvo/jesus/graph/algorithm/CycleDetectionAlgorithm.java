package salvo.jesus.graph.algorithm;
import salvo.jesus.graph.*;
import java.util.*;

/**
 * Abstract class for any algorithm implementing cycle detection on a
 * Graph (directed or undirected).  Methods act on either the entire graph or
 * specific vertices or edges.
 *
 * @author John V. Sichi
 */
public abstract class CycleDetectionAlgorithm
{
    protected Graph m_graph;

    /**
     * Example constructor for the algorithm.
     *
     * @param graph the Graph on which to operate
     */
    public CycleDetectionAlgorithm(Graph graph) 
    {
        m_graph = graph;
    }

    /**
     * Find the subgraph of all cycles.  The result is written into an existing
     * graph rather than returned in a new graph; this leaves the caller
     * control over the representation to use for the subgraph.
     *
     * @param subgraph an output parameter which receives the subgraph of all
     * vertices and edges which participate in cycles; on entry, should be
     * empty
     *
     * @exception Exception if subgraph.addEdge fails
     */
    public abstract void findCycleSubgraph(Graph subgraph)
        throws Exception;

    /**
     * Find the subgraph of all cycles which contain a particular vertex.
     * The result is written into an existing graph rather than returned
     * in a new graph; this leaves the caller control over the representation
     * to use for the subgraph.
     *
     * @param subgraph an output parameter which receives the subgraph of
     * all vertices and edges which participate in cycles containing v; on
     * entry, should be empty
     *
     * @param v the vertex to test
     *
     * @exception Exception if subgraph.addEdge fails
     */
    public abstract void findCycleSubgraph(Graph subgraph,Vertex v)
        throws Exception;

    /**
     * Find the subgraph of all cycles which contain a particular edge.
     * The result is written into an existing graph rather than returned
     * in a new graph; this leaves the caller control over the representation
     * to use for the subgraph.
     *
     * @param subgraph an output parameter which receives the subgraph of
     * all vertices and edges which participate in cycles containing e; on
     * entry, should be empty
     *
     * @param e the edge to test
     *
     * @exception Exception if subgraph.addEdge fails
     */
    public abstract void findCycleSubgraph(Graph subgraph,Edge e)
        throws Exception;

    /**
     * Perform cycle detection on the entire graph.
     *
     * @return true iff the graph contains at least one cycle
     */
    public abstract boolean detectCycles();

    /**
     * Determine whether an individual vertex is on a cycle.
     *
     * @param v the vertex to test
     *
     * @return true if v is on at least one cycle
     */
    public abstract boolean detectCycles(Vertex v);
    
    /**
     * Determine whether an individual edge is on a cycle.
     *
     * @param e the edge to test
     *
     * @return true if e is on at least one cycle
     */
    public abstract boolean detectCycles(Edge e);
}

// End CycleDetectionAlgorithm.java

package salvo.jesus.graph.algorithm;
import salvo.jesus.graph.*;
import java.util.*;

/**
 * An implementation of CycleDetectionAlgorithm via DFS.  Although
 * the interface declares parameters of type Graph, only DirectedGraphs are
 * currently supported.
 *
 * @author John V. Sichi
 */
public class CycleDetectionAlgorithmDFS extends CycleDetectionAlgorithm
{
    // TODO jvs 10-Feb-2002 -- most of these methods can be optimized
    // by remembering cyclic subgraphs and cycle-free subgraphs as we see them,
    // and skipping when encountered again

    public CycleDetectionAlgorithmDFS(Graph graph)
    {
        super(graph);
        if (!(graph instanceof DirectedGraph)) {
            // TODO jvs 10-Feb-2002 -- support undirected graphs
            throw new IllegalArgumentException(
                "cycle detection in undirected graphs not yet implemented");
        }
    }

    /**
     * @see CycleDetectionAlgorithm#findCycleSubgraph(Graph)
     */
    public void findCycleSubgraph(Graph subgraph)
        throws Exception
    {
        Iterator iterator = m_graph.getEdgeSet().iterator();
        while (iterator.hasNext()) {
            Edge e = (Edge) iterator.next();
            if (detectCycles(e)) {
                subgraph.addEdge(e);
            }
        }
    }

    /**
     * @see CycleDetectionAlgorithm#findCycleSubgraph(Graph,Vertex)
     */
    public void findCycleSubgraph(Graph subgraph,Vertex v)
        throws Exception
    {
        DirectedGraph directedGraph = (DirectedGraph) m_graph;
        Iterator iterator = m_graph.getEdgeSet().iterator();
        while (iterator.hasNext()) {
            DirectedEdge e = (DirectedEdge) iterator.next();
            if (directedGraph.isPath(e.getSink(),v)
                && directedGraph.isPath(v,e.getSource()))
            {
                subgraph.addEdge(e);
            }
        }
    }

    /**
     * @see CycleDetectionAlgorithm#findCycleSubgraph(Graph,Edge)
     */
    public void findCycleSubgraph(Graph subgraph,Edge e)
        throws Exception
    {
        DirectedEdge d1 = (DirectedEdge) e;
        DirectedGraph directedGraph = (DirectedGraph) m_graph;
        Iterator iterator = m_graph.getEdgeSet().iterator();
        while (iterator.hasNext()) {
            DirectedEdge d2 = (DirectedEdge) iterator.next();
            if (directedGraph.isPath(d1.getSink(),d2.getSource())
                && directedGraph.isPath(d2.getSink(),d1.getSource()))
            {
                subgraph.addEdge(d2);
            }
        }
    }

    /**
     * @see CycleDetectionAlgorithm#detectCycles()
     */
    public boolean detectCycles()
    {
        // TODO jvs 10-Feb-2002 -- this can be optimized by remembering
        // portions known to be cycle-free and skipping them

        Iterator vertexIter = m_graph.getVerticesIterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            if (detectCycles(v)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @see CycleDetectionAlgorithm#detectCycles(Vertex)
     */
    public boolean detectCycles(Vertex v)
    {
        return ((DirectedGraph) m_graph).isCycle(v);
    }
    
    /**
     * @see CycleDetectionAlgorithm#detectCycles(Edge)
     */
    public boolean detectCycles(Edge e)
    {
        DirectedEdge directedEdge = (DirectedEdge) e;
        return ((DirectedGraph) m_graph).isPath(
            directedEdge.getSink(),directedEdge.getSource());
    }
}

// End CycleDetectionAlgorithmDFS.java

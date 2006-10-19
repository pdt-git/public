package salvo.jesus.graph.adaptor;
import salvo.jesus.graph.*;
import salvo.jesus.graph.algorithm.*;
import java.util.*;

/**
 * DirectedGraphDelegator is a utility base for creating adaptors which need to
 * delegate most of their methods to an underlying DirectedGraph.
 * 
 * @author John V. Sichi
 */
public class DirectedGraphDelegator
    extends GraphDelegator implements DirectedGraph
{
    protected DirectedGraphDelegator(DirectedGraph graph)
    {
        super(graph);
    }
    
    protected final DirectedGraph getDirectedGraph()
    {
        return (DirectedGraph) super.getGraph();
    }

    public List getOutgoingEdges(Vertex v)
    {
        return getDirectedGraph().getOutgoingEdges(v);
    }

    public List getIncomingEdges(Vertex v)
    {
        return getDirectedGraph().getIncomingEdges(v);
    }

    public List getOutgoingAdjacentVertices(Vertex v)
    {
        return getDirectedGraph().getOutgoingAdjacentVertices(v);
    }

    public List getIncomingAdjacentVertices(Vertex v)
    {
        return getDirectedGraph().getIncomingAdjacentVertices(v);
    }

    public DirectedEdge getEdge(Vertex fromvertex,Vertex tovertex)
    {
        return getDirectedGraph().getEdge(fromvertex,tovertex);
    }

    public boolean isPath(Vertex fromvertex,Vertex tovertex)
    {
        return getDirectedGraph().isPath(fromvertex,tovertex);
    }

    public boolean isCycle(Vertex fromvertex)
    {
        return getDirectedGraph().isCycle(fromvertex);
    }
}

// End DirectedGraphDelegator.java

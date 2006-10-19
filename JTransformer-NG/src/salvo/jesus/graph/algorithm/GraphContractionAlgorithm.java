package salvo.jesus.graph.algorithm;
import salvo.jesus.graph.*;
import java.util.*;

/**
 * GraphContraction is an algorithm class for managing a series of contractions
 * on a graph (directed or undirected).  It is abstract because it requires a
 * concrete subclass to supply certain rules about how the contraction should
 * be performed.  Note that GraphContraction modifies its input graph in place;
 * it does not construct a new graph.
 *
 * @author John V. Sichi
 */
public abstract class GraphContractionAlgorithm
{
    private Graph m_graph;

    /**
     * A map from the original vertex and edge sets to the resulting sets after
     * contraction.
     */
    private Map m_contractionMap;
    
    protected GraphContractionAlgorithm(Graph graph)
    {
        m_graph = graph;
        m_contractionMap = new HashMap();
    }

    /**
     * @return the graph on which this contraction operates
     */
    public Graph getGraph()
    {
        return m_graph;
    }

    /**
     * Contract two arbitrary vertices within a graph.  The vertices need not
     * be already connected.
     *
     * @param vRemain the vertex that will remain after contraction
     *
     * @param vRemove the vertex that will be removed by contraction
     *
     * @exception Exception if the graph cannot be modified
     */
    public void contractVertexPair(Vertex vRemain,Vertex vRemove)
        throws Exception
    {
        // in case these vertices have already been contracted, get their
        // current mappings
        vRemove = getContractionVertex(vRemove);
        vRemain = getContractionVertex(vRemain);
        
        if (vRemove == vRemain) {
            // that's easy enough...but maybe should filter any self-loops
            // through shouldBeSelfLoop() as well?
            return;
        }
        // transfer all of vRemove's edges to vRemain
        List edgeList = m_graph.getEdges(vRemove);
        if (edgeList == null) {
            edgeList = Collections.EMPTY_LIST;
        }
        Iterator edges = edgeList.iterator();
        while (edges.hasNext()) {
            Edge edge = (Edge) edges.next();
            Vertex opposite = edge.getOppositeVertex(vRemove);
            if (opposite == vRemain) {
                if (!shouldBeSelfLoop(edge)) {
                    continue;
                } else {
                    // below will create a self-loop
                }
            }
            
            if (opposite == vRemove) {
                // there was already a self-loop
                if (shouldBeSelfLoop(edge)) {
                    // transfer the accepted self-loop to vRemain
                    opposite = vRemain;
                } else {
                    // this rejected self-loop will go away when
                    // we delete vRemove at the end
                }
            }

            // preserve direction in case it's relevant (assumes vertexA is
            // always the source for a DirectedEdge)
            Edge newEdge;
            if (vRemove == edge.getVertexA()) {
                newEdge = copyEdge(edge,vRemain,opposite);
            } else {
                newEdge = copyEdge(edge,opposite,vRemain);
            }
            // record the edge contraction
            m_contractionMap.put(edge,newEdge);
        }

        // now lose vRemove
        m_graph.remove(vRemove);

        // record the vertex contraction
        m_contractionMap.put(vRemove,vRemain);
    }

    /**
     * Contract an arbitrary collection of vertices within a graph into a
     * single vertex. The vertices need not be already connected.
     *
     * @param vertices the vertices to be contracted; the first vertex returned
     * by this Collection's iterator will be the one to remain
     *
     * @exception Exception if the graph cannot be modified
     */
    public void contractVertices(Collection vertices)
        throws Exception
    {
        // Contraction is transitive, so we can do it willy-nilly by just
        // iterating over vertices and contracting each vertex
        // with the first one.
        Iterator iter = vertices.iterator();
        if (!iter.hasNext()) {
            return;
        }
        Vertex vRemain = (Vertex) iter.next();
        while (iter.hasNext()) {
            Vertex vRemove = (Vertex) iter.next();
            contractVertexPair(vRemain,vRemove);
        }
    }
    
    /**
     * Perform contractVertexPair for the two incident vertices of
     * each edge in a specified set.  For each edge, getVertexA() determines
     * vRemain and getVertexB() determines vRemove.
     *
     * @param edges the edges to contract
     *
     * @exception Exception if the graph cannot be modified
     */
    public void contractEdges(Collection edges)
        throws Exception
    {
        Iterator edgeIter = edges.iterator();
        while (edgeIter.hasNext()) {
            Edge edge = (Edge) edgeIter.next();
            contractVertexPair(edge.getVertexA(),edge.getVertexB());
        }
    }

    /**
     * Find the resulting contraction for a Vertex.
     *
     * @param v the Vertex to find the contraction for
     *
     * @return the contracted vertex (same as v if v has not been contracted)
     */
    public Vertex getContractionVertex(Vertex v)
    {
        return (Vertex) getContractionResult(v);
    }

    /**
     * Find the resulting contraction for an Edge.
     *
     * @param edge the Edge to find the contraction for
     *
     * @return the contracted edge (same as edge if edge has not been
     * contracted)
     */
    public Edge getContractionEdge(Edge edge)
    {
        return (Edge) getContractionResult(edge);
    }

    /**
     * Find the resulting contraction for a GraphComponent
     * (Vertex or Edge).
     *
     * @param c the GraphComponent
     *
     * @return the contracted component (same as c if c has not been
     * contracted)
     */
    public GraphComponent getContractionResult(GraphComponent c)
    {
        // a little bit of union-find fun
        GraphComponent c2 = (GraphComponent) m_contractionMap.get(c);
        if (c2 == null) {
            return c;
        }
        GraphComponent c3 = getContractionResult(c2);
        if (c3 != c2) {
            m_contractionMap.put(c,c3);
        }
        return c3;
    }
    
    /**
     * Subclass method called to decide whether a given edge
     * encountered during contraction should be turned into a self-loop
     * on the contracted vertex.
     */
    protected abstract boolean shouldBeSelfLoop(Edge e);

    /**
     * Subclass method called to add a new contracted edge equivalent to
     * oldEdge, but connecting vA to vB.  The rule may decide that oldEdge is
     * already equivalent to an existing edge between vA and vB, in which case
     * no new edge is added.  If a new edge is created, the implementation
     * should add it to getGraph().
     *
     * @param oldEdge the edge being contracted
     *
     * @param vA one vertex to which the new edge should be incident;
     * for a DirectedGraph, this is the source
     *
     * @param vB the other vertex to which the new edge should be incident;
     * for a DirectedGraph, this is the sink
     *
     * @return the new or existing edge
     *
     * @exception Exception if the graph cannot be modified
     */
    protected abstract Edge copyEdge(Edge oldEdge,Vertex vA,Vertex vB)
        throws Exception;
}

// End GraphContractionAlgorithm.java

package salvo.jesus.graph.algorithm;
import salvo.jesus.graph.*;
import java.util.*;

/**
 * GraphUnionAlgorithm is an algorithm class for managing the operation
 * of taking the union of graphs (directed or undirected).  The equivalence
 * function used to combine graph components is the object identity of the
 * vertices and edges themselves.
 *
 * @author John V. Sichi
 */
public class GraphUnionAlgorithm
{
    private Graph m_result;
    
    /**
     * Begin a new union.  The union will be written to the graph specified as
     * a parameter, which need not be empty to start with.
     *
     * @param gResult the union target, which will be modified in place
     */
    public GraphUnionAlgorithm(Graph gResult)
    {
        m_result = gResult;
    }

    /**
     * @return the union result graph specified in the constructor
     */
    public Graph getGraph()
    {
        return m_result;
    }

    /**
     * Merge a graph into the union.
     *
     * @param gSrc the graph to add; this graph is unmodified by the operation
     *
     * @exception Exception if the result graph cannot be modified
     */
    public void union(Graph gSrc)
        throws Exception
    {
        // first pass:  copy all the vertex references; graph will take
        // care of duplicates
        Iterator vertexIter = gSrc.getVerticesIterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            m_result.add(v);
        }

        // second pass:  copy all the edge references; graph will take
        // care of duplicates
        vertexIter = gSrc.getVerticesIterator();
        while (vertexIter.hasNext()) {
            Vertex v = (Vertex) vertexIter.next();
            Iterator edgeIter = gSrc.getEdges(v).iterator();
            while (edgeIter.hasNext()) {
                Edge edge = (Edge) edgeIter.next();
                if (edge.getVertexA() == v) {
                    m_result.addEdge(edge);
                }
            }
        }
    }
}

// End GraphUnionAlgorithm.java
